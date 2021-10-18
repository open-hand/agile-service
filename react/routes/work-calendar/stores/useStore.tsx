import { useLocalStore } from 'mobx-react-lite';
import { map } from 'lodash';
import { Issue as OldIssue } from '@/common/types';
import {
  formatDate, formatIssueData,
} from '@/routes/work-calendar/utils';
import {
  CalendarRefPros, UserValueCode, IssueItem,
} from '@/routes/work-calendar/types';
import { orgWorkCalendarApi } from '@/api/OrgWorkCalendar';

interface Issue extends OldIssue{
  id: string,
  title: string,
  start: Date,
  end: Date,
}

interface Props {
  DEFAULT_USER: UserValueCode[],
}

export default function useStore({ DEFAULT_USER }: Props) {
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

    filterIssueId: null,
    get getFilterIssueId() {
      return this.filterIssueId;
    },
    setFilterIssueId(data: string | null) {
      this.filterIssueId = data;
    },

    issueList: [],
    get getIssueList() {
      return this.issueList;
    },
    setIssueList(data: IssueItem[]) {
      this.issueList = data;
    },

    expandMap: new Map(),

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
          filterIssueId: this.filterIssueId,
          startTime: formatDate(start),
          endTime: formatDate(end),
        };
        const issues = await orgWorkCalendarApi.loadIssueByDate(postData);
        const newIssues = map(issues || [], (item) => formatIssueData(item));
        this.setIssues(newIssues);
        return newIssues;
      } catch (e) {
        return [];
      }
    },

    async loadIssueList() {
      try {
        const postData = {
          assigneeFilter: this.users,
          projectIds: this.currentProjectIds,
        };
        const res = await orgWorkCalendarApi.loadIssueList(postData);
        const newData = map(res, (item) => ({
          ...item,
          completedCount: item.countVO?.completedCount ?? 0,
          totalCount: item.countVO?.totalCount ?? 0,
        }));
        this.setIssueList(newData);
        return newData;
      } catch (e) {
        return [];
      }
    },
  }));
}

export type StoreProps = ReturnType<typeof useStore>;
