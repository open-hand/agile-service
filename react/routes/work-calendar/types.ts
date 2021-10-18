import { CalendarApi } from '@fullcalendar/common';
import { Issue, IStatus } from '@/common/types';

interface CalendarRefPros {
  getApi(): CalendarApi,
}

interface IssueItem extends Issue{
  completedCount: number,
  totalCount: number
  projectId: string,
}

interface DefaultValuesProps {
  estimatedStartTime: string,
  estimatedEndTime: string
}

interface CreateProps {
  defaultValues: DefaultValuesProps,
  clearSelect?: boolean,
}

type StatusProps = {
  [key in IStatus['valueCode']]: string
}

type UserValueCode = 'assignee' | 'participant';

export {
  CalendarRefPros,
  StatusProps,
  UserValueCode,
  IssueItem,
  CreateProps,
};
