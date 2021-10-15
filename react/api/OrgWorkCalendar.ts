import { getOrganizationId } from '@/utils/common';
import Api from './Api';

class OrgWorkCalendarApi extends Api<OrgWorkCalendarApi> {
  get prefix() {
    return `/agile/v1/organizations/${getOrganizationId()}/work_calender`;
  }

  /**
   * 工作日历中查询issue
   * @param data
   */
  loadIssueByDate(data: object) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/query_assignee_issue`,
      data,
    });
  }

  /**
   * 工作日历中查询当前迭代父级issue
   * @param data
   */
  loadIssueList(data: object) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/query_parent_issue`,
      data,
    });
  }
}

const orgWorkCalendarApi = new OrgWorkCalendarApi();
const orgWorkCalendarConfigApi = new OrgWorkCalendarApi(true);
export { orgWorkCalendarApi, orgWorkCalendarConfigApi };
