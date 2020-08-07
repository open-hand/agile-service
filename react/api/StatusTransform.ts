import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import { IStatus } from '@/common/types';

export interface IStatusCirculation {
  code: string
  defaultStatus: boolean
  id: string
  name: string
  stateMachineId: string
  type: IStatus['valueCode']
  canTransformStatus: string[];
  default?: boolean
  [propName: string]: any
}

class StatusTransformApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  loadList(issueTypeId: string): Promise<IStatusCirculation[]> {
    return axios({
      method: 'get',
      url: `${this.prefix}/status_transform/list`,
      params: {
        applyType: 'agile',
        issueTypeId,
      },
    });
  }

  setDefaultStatus(issueTypeId:string, statusId:string, stateMachineId:string) {
    return axios({
      method: 'put',
      url: `${this.prefix}/status_transform/default_status`,
      params: {
        issueTypeId,
        statusId,
        stateMachineId,
      },
    });
  }
}

const statusTransformApi = new StatusTransformApi();
export { statusTransformApi };
