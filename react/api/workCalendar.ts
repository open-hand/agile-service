import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

interface IWorkCalendar {
    status:number,
    workDay:string, // "1999-09-09"
}
/**
 * 冲刺的工作日历 api
 * @author ding
 */
class WorkCalendarApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
   * 根据年份获取冲刺工作日历设置
   * @param year //1999
   */
  getCalendar(year:number) {
    return axios({
      method: 'get',
      url: `${this.prefix}/work_calendar_ref/sprint`,
      params: {
        year,
      },
    });
  }

  /**
   * 创建工作日历
   * @param sprintId 
   * @param data 
   */
  create(sprintId: number, data:IWorkCalendar) {
    return axios.post(`${this.prefix}/work_calendar_ref/sprint/${sprintId}`, data);
  }

  /**
       * 删除工作日历
       * @param calendarId 
       */
  delete(calendarId: number) {
    return axios.delete(`${this.prefix}/work_calendar_ref/${calendarId}`);
  }
}

const workCalendarApi = new WorkCalendarApi();
// eslint-disable-next-line import/prefer-default-export
export { workCalendarApi };
