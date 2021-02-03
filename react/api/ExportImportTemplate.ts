import { stores } from '@choerodon/boot';

import Api from './Api';

const { AppState } = stores;

export type TemplateAction = 'agile_import_issue' | 'agile_export_issue' | 'program_import_feature' | 'program_export_feature' | 'agile_export_feature';

class TemplateApi extends Api<TemplateApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  getList(action: TemplateAction) {
    const userId = AppState.userInfo.id;
    return this.request({
      method: 'get',
      url: `${this.prefix}/personal_template/user/${userId}`,
      params: {
        action,
        type: 'excel',
      },
    });
  }

  delete(templateId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/personal_template/${templateId}`,
    });
  }

  create(data: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/personal_template`,
      data,
    });
  }

  edit(templateId: string, data:any) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/personal_template/${templateId}`,
      data,
    });
  }

  checkName(name: string, action: TemplateAction) {
    const userId = AppState.userInfo.id;
    return this.request({
      method: 'get',
      url: `${this.prefix}/personal_template/check_name`,
      params: {
        name,
        userId,
        action,
        type: 'excel',
      },
    });
  }

  getTemplate(templateId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/static_file/related`,
    });
  }
}

const templateApi = new TemplateApi();

export { templateApi };
