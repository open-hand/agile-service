import { CalendarApi } from '@fullcalendar/common';
import { IStatus } from '@/common/types';

interface CalendarRefPros {
  getApi(): CalendarApi,
}

type StatusProps = {
  [key in IStatus['valueCode']]: string
}

type UserValueCode = 'assignee' | 'participant';

export {
  CalendarRefPros,
  StatusProps,
  UserValueCode,
};
