package scalawave.service

import cats.{Functor, Monad}
import cats.data.State
import cats.implicits._

import scala.concurrent.Future
import scalawave.db.interpreter.PureKVSInterpreter
import scalawave.model._
import scalawave.repository.JobRepository
import scalawave.repository.interpreter.JobRepoKVInterp

trait JobService[F[_]] {

  def create(id: JobId, accountId: AccountId, location: Location): F[Job]

  def addRequiredSkills(id: JobId, skills: SkillTag*): F[Option[Job]]
}

abstract class JobServiceInterp[F[_] : Monad]
  extends JobService[F] {

  val repo: JobRepository[F]

  override def create(id: JobId, accountId: AccountId, location: Location): F[Job] = {

    val j = Job(id, accountId, location, skills = None)
    repo.store(Job(id, accountId, location, skills = None)).map { _ =>
      j
    }
  }

  override def addRequiredSkills(id: JobId, skills: SkillTag*): F[Option[Job]] = {
    repo.query(id).flatMap { maybeJob =>
      val maybeUpdatedJob = maybeJob.map { job =>
        val newSkills = job.skills.map { currentSkills =>
          currentSkills ++ skills
        }.orElse(Some(skills))

        job.copy(skills = newSkills)
      }

      val x: F[Option[Job]] = maybeUpdatedJob.traverse { job =>
        repo.store(job).map(_ => job)
      }

      val y: Option[F[Job]] = maybeUpdatedJob.map { job =>
        repo.store(job).map(_ => job)
      }

      x
    }
  }
}

object Tst extends App {

  val jobRepo: JobRepoKVInterp[State[Map[JobId, Job], ?]] = {
    JobRepoKVInterp(PureKVSInterpreter.interpreter)
  }

  val service = new JobServiceInterp[State[Map[JobId, Job], ?]] {
    override val repo = jobRepo
  }

  val state = for {
    _ <- service.create(JobId("job1"), AccountId("acc1"), Location(Latitude(42.0), Longitude(42.0)))
    s <- service.create(JobId("job2"), AccountId("acc2"), Location(Latitude(42.0), Longitude(42.0)))
  } yield s

  val value: (Map[JobId, Job], Job) = state.run(Map()).value
  println(value)
}
