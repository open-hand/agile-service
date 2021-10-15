import { useLocalStore } from 'mobx-react-lite';
import { axios } from '@choerodon/boot';
import { map, assign } from 'lodash';
import moment from 'moment';
import { getOrganizationId } from '@/utils/common';
import { Issue as OldIssue } from '@/common/types';
import { formatIssueTime, formatDate } from '@/routes/work-calendar/utils';
import { CalendarRefPros, StatusProps, UserValueCode } from '@/routes/work-calendar/types';

interface Issue extends OldIssue{
  id: string,
  title: string,
  start: Date,
  end: Date,
}

interface Props {
  STATUS: StatusProps,
  DEFAULT_USER: UserValueCode[],
}

export default function useStore({ STATUS, DEFAULT_USER }: Props) {
  return useLocalStore(() => ({
    calendarRef: null,
    get getCalendarRef() {
      return this.calendarRef;
    },
    setCalendarRef(ref: CalendarRefPros) {
      this.calendarRef = ref;
    },

    users: DEFAULT_USER,
    get getUsers() {
      return this.users;
    },
    setUsers(data: UserValueCode[]) {
      this.users = data;
    },

    currentProjectIds: null,
    get getCurrentProjectIds() {
      return this.currentProjectIds;
    },
    setCurrentProjectIds(data: string[]) {
      this.currentProjectIds = data;
    },

    issues: [],
    get getIssues() {
      return this.issues.slice();
    },
    setIssues(data: Issue[]) {
      this.issues = data;
    },
    async loadIssues({ start, end }: { start: Date, end: Date }) {
      try {
        const postData = {
          assigneeFilter: this.users,
          projectIds: this.currentProjectIds,
          startTime: formatDate(start),
          endTime: formatDate(end),
        };
        const issues = await axios.post(`/agile/v1/organizations/${getOrganizationId()}/work_calender/query_assignee_issue`, postData);
        const newIssues = map(issues || [], (item) => {
          const startTime = moment(item.estimatedEndTime);
          const allDay = startTime.diff(moment(item.estimatedStartTime), 'hours') % 24 === 0 && startTime.hours() === 0 && startTime.minutes() === 0;
          return assign(item, {
            id: item.issueId,
            start: formatIssueTime(item.estimatedStartTime),
            end: formatIssueTime(item.estimatedEndTime),
            title: item.summary,
            // @ts-ignore
            backgroundColor: STATUS[item.statusVO?.type || 'todo'],
            borderColor: item.priorityVO?.colour || 'transparent',
            allDay,
          });
        });
        this.setIssues(newIssues);
        return newIssues;
      } catch (e) {
        return [];
      }
    },
  }));
}

export type StoreProps = ReturnType<typeof useStore>;
