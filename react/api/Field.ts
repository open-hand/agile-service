import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

interface QuickCreateDefault {
    context: string, // "task"
    pageCode: string, // "agile_issue_create"
    schemeCode: string, // "agile_issue"
}
class FieldApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
     * 快速创建字段默认值
     * @param issueId 
     * @param dto 
     */
  quickCreateDefault(issueId: number, dto: QuickCreateDefault) {
    return axios.post(`${this.prefix}/field_value/quick_create/${issueId}`, dto);
  }
}

const fieldApi = new FieldApi();
export { fieldApi };
