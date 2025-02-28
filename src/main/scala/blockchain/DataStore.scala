package blockchain

import cats.Applicative
import cats.implicits._

import scala.collection.mutable

trait DataStore[F[_], K, V] {
  def put(key: K, value: V): F[Unit]
  def get(key: K): F[Option[V]]
}

class InMemoryDataStore[F[_]: Applicative, K, V](initial: Map[K, V] = Map.empty) extends DataStore[F, K, V] {
  private val storage: mutable.HashMap[K, V]  = mutable.HashMap.from(initial)
  override def put(key: K, value: V): F[Unit] = storage.put(key, value).pure[F].void

  override def get(key: K): F[Option[V]] = storage.get(key).pure[F]
}
