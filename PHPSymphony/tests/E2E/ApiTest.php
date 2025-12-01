<?php

namespace App\Tests\E2E;

use Symfony\Bundle\FrameworkBundle\Test\WebTestCase;

class ApiTest extends WebTestCase
{
    // These tests would require the server to be running or using the internal Client.
    // Assuming the test environment is set up.

    /*
    public function testHealth()
    {
        $client = static::createClient();
        $client->request('GET', '/health');

        $this->assertResponseIsSuccessful();
        $this->assertJsonStringEqualsJsonString(
            json_encode(['status' => 'ok', 'version' => '1.0.0']),
            $client->getResponse()->getContent()
        );
    }
    */
}
