package scalawave.repository

import cats.Monad
import cats.data.{Kleisli, ReaderT, State}
import org.scalatest._

import scalawave.db.interpreter.PureKVSInterpreter
import scalawave.model._
import scalawave.repository.interpreter._

class InitiallyFinalSpec extends FlatSpec with Matchers {

  val resourceRepo: ResourceRepoKVInterp[State[Map[ResourceId, Resource], ?]] = {
    ResourceRepoKVInterp(PureKVSInterpreter.interpreter)
  }

  val accountRepo: AccountRepoKVInterp[State[Map[AccountId, Account], ?]] = {
    AccountRepoKVInterp(PureKVSInterpreter.interpreter)
  }

  val jobRepo: JobRepoKVInterp[State[Map[JobId, Job], ?]] = {
    JobRepoKVInterp(PureKVSInterpreter.interpreter)
  }

  "A repository" should "store and retrieve it's respective objects" in {

    def store[K, V <: HasUID[K], F[_]](repo: Repository[K, V, F]): ReaderT[F, V, Unit] = {
      ReaderT(v => repo.store(v))
    }

    def retrieve[K, V <: HasUID[K], F[_]](repo: Repository[K, V, F]): ReaderT[F, V, Option[V]] = {
      ReaderT(v => repo.query(v.uid))
    }

    def storeAndRetrieve[K, V <: HasUID[K], F[_] : Monad](repo: Repository[K, V, F]): Kleisli[F, V, Option[V]] = {
      store(repo).flatMap(_ => retrieve(repo))
    }

    val location = Location(Latitude(42.0), Longitude(42.0))

    val job = Job(
      JobId("job1"),
      AccountId("account1"),
      location,
      None
    )

    val resource = Resource(
      ResourceId("res1"),
      location
    )

    val account = Account(AccountId("acc1"), "Scalac")

    val accountStorage = storeAndRetrieve(accountRepo)
    val jobStorage = storeAndRetrieve(jobRepo)
    val resourceStorage = storeAndRetrieve(resourceRepo)

    jobStorage(job).run(Map()).value._2 should be(Some(job))
    resourceStorage(resource).run(Map()).value._2 should be(Some(resource))
    accountStorage(account).run(Map()).value._2 should be(Some(account))
  }

  "A job repository" should "retrieve jobs for specific skills" in {
    val skill1 = SkillTag(TagId("skill1"), "skill 1")
    val skill2 = SkillTag(TagId("skill2"), "skill 2")

    val acc = AccountId("acc1")
    val location = Location(Latitude(42.0), Longitude(42.0))

    val job1 = Job(JobId("job1"), acc, location, skills = Some(Vector(skill1, skill2)))
    val job2 = Job(JobId("job2"), acc, location, skills = Some(Vector(skill1)))
    val job3 = Job(JobId("job3"), acc, location, skills = Some(Vector(skill2)))


    val operation = for {
      _ <- jobRepo.store(job1)
      _ <- jobRepo.store(job2)
      _ <- jobRepo.store(job3)
      res <- jobRepo.forSkills(skill1)
    } yield res

    operation.run(Map()).value._2 shouldBe Seq(job1, job2)
  }

  "A job repository" should "retrieve jobs for specific accounts" in {
    val skill = SkillTag(TagId("skill1"), "skill 1")

    val acc1 = AccountId("acc1")
    val acc2 = AccountId("acc2")

    val location = Location(Latitude(42.0), Longitude(42.0))

    val job1 = Job(JobId("job1"), acc1, location, skills = None)
    val job2 = Job(JobId("job2"), acc1, location, skills = None)
    val job3 = Job(JobId("job3"), acc2, location, skills = None)

    val operation = for {
      _ <- jobRepo.store(job1)
      _ <- jobRepo.store(job2)
      _ <- jobRepo.store(job3)
      res <- jobRepo.forAccount(acc1)
    } yield res

    operation.run(Map()).value._2 shouldBe Seq(job1, job2)
  }

  "A resource repository" should "retrieve resource having specific skills" in {

    val location = Location(Latitude(42.0), Longitude(42.0))
    val skill1 = SkillTag(TagId("skill1"), "skill 1")
    val skill2 = SkillTag(TagId("skill2"), "skill 2")

    val res1 = Resource(ResourceId("res1"), location, skills = Some(Vector(skill1, skill2)))
    val res2 = Resource(ResourceId("res2"), location, skills = Some(Vector(skill1)))
    val res3 = Resource(ResourceId("res3"), location, skills = Some(Vector(skill2)))

    val operation = for {
      _ <- resourceRepo.store(res1)
      _ <- resourceRepo.store(res2)
      _ <- resourceRepo.store(res3)
      res <- resourceRepo.withSkills(skill1)
    } yield res

    operation.run(Map()).value._2 shouldBe Seq(res1, res2)
  }
}
