import { useLocalStore } from 'mobx-react-lite';
import { map } from 'lodash';
import moment from 'moment';
import {
  formatDate, formatIssueData,
} from '@/routes/work-calendar/utils';
import {
  CalendarRefPros, UserValueCode, IssueItem, ViewTypeCode,
} from '@/routes/work-calendar/types';
import { orgWorkCalendarApi } from '@/api/OrgWorkCalendar';

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
      return this.issueList?.slice();
    },
    setIssueList(data: IssueItem[]) {
      this.issueList = data;
    },

    expandMap: new Map(),

    issueSearchParams: '',
    get getIssueSearchParams() {
      return this.issueSearchParams;
    },
    setIssueSearchParams(data: string) {
      this.issueSearchParams = data;
    },

    issueListLoading: true,
    get getIssueListLoading() {
      return this.issueListLoading;
    },
    setIssueListLoading(flag: boolean) {
      this.issueListLoading = flag;
    },

    currentViewType: 'timeGridWeek',
    get getCurrentViewType() {
      return this.currentViewType;
    },
    setCurrentViewType(data: ViewTypeCode) {
      this.currentViewType = data;
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
        return newIssues;
      } catch (e) {
        return [];
      }
    },

    async loadIssueList() {
      try {
        this.setIssueListLoading(true);
        const postData = {
          assigneeFilter: this.users,
          projectIds: this.currentProjectIds,
          params: this.searchParams,
        };
        const res = await orgWorkCalendarApi.loadIssueList(postData);
        const newData = map(res, (item) => ({
          ...item,
          estimatedStartTime: moment(item.estimatedStartTime).format('YYYY-MM-DD HH:mm'),
          estimatedEndTime: moment(item.estimatedEndTime).format('YYYY-MM-DD HH:mm'),
          completedCount: item.countVO?.completedCount ?? 0,
          totalCount: item.countVO?.totalCount ?? 0,
        }));
        this.setIssueListLoading(false);
        this.setIssueList(newData);
        return newData;
      } catch (e) {
        this.setIssueListLoading(false);
        return [];
      }
    },
  }));
}

export type StoreProps = ReturnType<typeof useStore>;
