import { Controller, Get } from '@nestjs/common';

@Controller('health')
export class HealthController {
  /**
   * Checks the status and returns an object with a status message.
   */
  @Get()
  check() {
    return { status: 'ok' };
  }
}
