import queryString from 'querystring';
import { find } from 'lodash';
import { stores } from '@choerodon/boot';
import { History } from 'history';
import { error } from './log';

const { HeaderStore, AppState } = stores;
let history: History | null;

export function setHistory(newHistory: History) {
  history = newHistory;
}
interface IProject {
  id: number
  name: string
  category: 'GENERAL' | 'PROGRAM'
  organizationId: number
}

interface IOrg {
  id: number
  name: string
}
type Path = string;
type ProjectLocationDescriptor = {
  type: 'project'
  id?: number
  name?: string
  category?: 'GENERAL' | 'PROGRAM'
  organizationId?: number
  params?: {
    [param: string]: string
  }
}
type OrgLocationDescriptor = {
  type: 'org'
  id?: number
  name?: string
  organizationId?: number
  params?: {
    [param: string]: string
  }
}
type SiteLocationDescriptor = {
  type: 'site'
  organizationId?: number
  params?: {
    [param: string]: string
  }
}
type LocationDescriptor =
  ProjectLocationDescriptor |
  OrgLocationDescriptor |
  SiteLocationDescriptor;
const defaultDescriptor: ProjectLocationDescriptor = {
  type: 'project',
};
const to = (path: Path, descriptor: LocationDescriptor = defaultDescriptor) => {
  const { type, params: otherParams } = descriptor;
  let params;
  switch (type) {
    case 'project': {
      const { id } = descriptor as ProjectLocationDescriptor;
      if (!id) {
        const {
          id: projectId, name, category, organizationId,
        } = AppState.currentMenuType;
        params = {
          type: 'project',
          id: String(projectId),
          name,
          category,
          organizationId: String(organizationId),
        };
      } else {
        const projects: IProject[] = HeaderStore.getProData;
        const targetProject = find(projects, (v) => String(v.id) === String(id));
        if (!targetProject) {
          error('跳转错误，未找到目标项目，请检查参数', path, descriptor);
          return;
        }
        const {
          name,
          category,
          organizationId,
        } = targetProject;
        params = {
          type: 'project',
          id: String(id),
          name,
          category,
          organizationId: String(organizationId),
        };
      }
      break;
    }
    case 'org': {
      const { id } = descriptor as OrgLocationDescriptor;
      if (!id) {
        const {
          id: orgId, name,
        } = AppState.currentMenuType;
        params = {
          type: 'organization',
          id: String(orgId),
          name,
          organizationId: String(orgId),
        };
      } else {
        const orgs: IOrg[] = HeaderStore.getOrgData;
        const targetOrg = find(orgs, (v) => String(v.id) === String(id));
        if (!targetOrg) {
          error('跳转错误，未找到目标组织，请检查参数', path, descriptor);
          return;
        }
        const {
          name,
        } = targetOrg;
        params = {
          type: 'organization',
          id: String(id),
          name,
          organizationId: String(id),
        };
      }
      break;
    }
    case 'site': {
      const {
        organizationId =
        AppState.currentMenuType.organizationId,
      } = descriptor as SiteLocationDescriptor;
      const orgs: IOrg[] = HeaderStore.getOrgData;
      const targetOrg = find(orgs, (v) => String(v.id) === String(organizationId));
      if (!targetOrg) {
        error('跳转错误，未找到目标组织，请检查参数', path, descriptor);
        return;
      }
      params = {
        type: 'site',
        organizationId: String(organizationId),
      };
      break;
    }
    default: {
      error('跳转错误，请检查参数', path, descriptor);
      break;
    }
  }
  const totalParams = {
    ...params,
    ...otherParams,
  };
  const search = queryString.stringify(totalParams);
  if (!history) {
    error('跳转失败，未设置history');
    return;
  }
  history.push({
    pathname: path,
    search,
  });
};

export default to;
