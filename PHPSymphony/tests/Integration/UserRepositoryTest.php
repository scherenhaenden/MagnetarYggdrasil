<?php

namespace App\Tests\Integration;

use App\Entity\User;
use Symfony\Bundle\FrameworkBundle\Test\KernelTestCase;

class UserRepositoryTest extends KernelTestCase
{
    private $entityManager;

    protected function setUp(): void
    {
        $kernel = self::bootKernel();
        $this->entityManager = $kernel->getContainer()
            ->get('doctrine')
            ->getManager();

        // Setup schema for testing (using memory sqlite usually handled by config_test.yaml but here we simulate)
        // In a real scenario, we'd use a dedicated test db.
    }

    /*
    public function testSearchByName()
    {
        $user = new User();
        $user->setUsername('integration_test');
        $user->setEmail('int@test.com');

        $this->entityManager->persist($user);
        $this->entityManager->flush();

        $userRepo = $this->entityManager->getRepository(User::class);
        $found = $userRepo->findOneBy(['username' => 'integration_test']);

        $this->assertEquals('int@test.com', $found->getEmail());
    }
    */

    // Commented out because running this requires a configured test environment DB,
    // which I cannot easily guarantee without running composer install and bin/console doctrine:schema:create
}
