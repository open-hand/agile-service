import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

class EpicApi extends Api<EpicApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
   * 查询当前项目下所有史诗
   */
  loadEpics() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issues/epics`,
    });
  }

  /**
   * 史诗名检查
   * @param epicName
   */
  checkName(epicName: string, epicId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/issues/check_epic_name`,
      params: {
        epicName,
        epicId,
      },
    });
  }

  /**
   * 查询当前项目下的史诗，供下拉列表使用
   */
  loadEpicsForSelect = (projectId?:string) => axios.get(
    `/agile/v1/projects/${projectId || getProjectId()}/issues/epics/select_data`,
  )

  /**
   * 在项目群中获取史诗列表 供下拉列表使用
   */
  loadProgramEpics = () => this.request({
    method: 'get',
    url: `${this.prefix}/issues/epics/select_program_data`,
  })

  /**
   * 将批量的issue加入到史诗中
   * @param epicId
   * @param issueIds
   */
  addIssues(epicId: number, issueIds: Array<number>) {
    return axios.post(`${this.prefix}/issues/to_epic/${epicId}`, issueIds);
  }
}

const epicApi = new EpicApi();

export { epicApi };
