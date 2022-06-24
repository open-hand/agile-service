import React, { useState, useEffect } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { EmptyPage, FlatSelect } from '@choerodon/components';
import { find } from 'lodash';

import TableCache from '@/components/table-cache';
import useIsInProgram from '@/hooks/useIsInProgram';
import { getMenuType, getProjectId, getUserId } from '@/utils/common';
import { ganttApi, projectApi } from '@/api';
import noDataPic from '@/assets/image/NoData.svg';
import Gantt from './stores';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import useDefaultMyFilter from '@/hooks/useDefaultMyFilter';
import { Loading } from '@/components';

// eslint-disable-next-line react/require-default-props
const GanttProject: React.FC<{ projectId: string, menuType?: 'project' | 'org', HeadSelect?: JSX.Element, [key: string]: any }> = ({
  projectId, projects, setCurrentProject, menuType = 'project', newItem, setCurrentItem,
}) => {
  const { isInProgram, loading: programLoading } = useIsInProgram({ projectId, menuType: 'project' });
  const { data: myFilter, isLoading } = useDefaultMyFilter({ projectId, menuType: 'project' });
  const loading = programLoading || isLoading;
  if (menuType === 'project') {
    return !loading ? (
      <TableCache type="gantt" projectId={projectId}>
        {(cacheProps) => <Gantt myDefaultFilter={myFilter} newItem={newItem} {...cacheProps} isInProgram={isInProgram} menuType="project" projectId={projectId} />}
      </TableCache>
    ) : <span />;
  }
  return projects.length && projectId && !loading ? (
    <TableCache type="gantt" projectId={projectId}>
      {(cacheProps) => (
        <Gantt myDefaultFilter={myFilter} setCurrentItem={setCurrentItem} {...cacheProps} newItem={newItem} isInProgram={isInProgram} menuType="org" projectId={projectId} projects={projects} setCurrentProject={setCurrentProject} />
      )}
    </TableCache>
  ) : null;
};

/**
 * 高阶包裹  当项目加载好  再去装载 Element
 * @param Element
 */

export const warpGanttProvideProjects = (Element: React.FC<{ projects: any[], setCurrentItem:(val:any)=>void, newItem:any, currentProjectId?: string, setCurrentProjectId: (val?: string | ((oldValue?: string) => string | undefined)) => void }>, level: 'org' | 'workbench' = 'org'): typeof React.PureComponent => class GanttProjectsProvider extends React.PureComponent<{ organizationId?: string }, { projects: any[], newItem:any, organizationId?: string, currentProjectId?: string, loading: boolean }> {
  constructor(props: any) {
    super(props);
    this.state = {
      projects: [],
      currentProjectId: undefined,
      organizationId: props.organizationId,
      loading: true,
      newItem: undefined,
    };
  }

  loadData = async () => {
    const { organizationId } = this.props;
    let projects;
    if (level === 'org') {
      projects = await ganttApi.org(organizationId).loadProjects();
    } else {
      projects = await projectApi.org(organizationId).loadProjectByUser({
        userId: getUserId(), page: 1, size: 0, category: ['N_AGILE', 'N_WATERFALL'],
      }).then((res: any) => res.list);
    }
    const cacheProjectId = localPageCacheStore.getItem('org.gantt.projectId');
    const cacheItem = localPageCacheStore.getItem('org.gantt.projectItem');
    const newProjects = projects.map((i: any) => ({ ...i, id: String(i.id) }));
    const newProjectId = (find(newProjects, { id: cacheProjectId }) || newProjects[0])?.id;
    const newItem = cacheItem || newProjects[0];
    localPageCacheStore.setItem('org.gantt.projectId', newProjectId);
    this.setState({
      projects: newProjects,
      currentProjectId: newProjectId,
      loading: false,
      newItem,
    });
  }

  static getDerivedStateFromProps(nextProps: any, prevState: any) {
    if (nextProps.organizationId !== prevState.organizationId) {
      return {
        organizationId: nextProps.organizationId, loading: true, projects: [], currentProjectId: undefined,
      };
    }
    return null;
  }

  componentDidUpdate() {
    if (this.state.loading && !this.state.projects.length) {
      this.loadData();
    }
  }

  componentDidMount() {
    this.loadData();
  }

  setCurrentProjectId = (value?: string | ((oldValue?: string) => string | undefined)) => {
    this.setState((pre) => {
      let newValue = value as string | undefined;
      if (typeof value === 'function') {
        newValue = value(pre.currentProjectId);
      }
      return { currentProjectId: newValue };
    });
  }

  setCurrentItem= (item:any) => {
    this.setState((pre) => ({ newItem: item }));
  }

  render() {
    const {
      loading, currentProjectId, projects, newItem,
    } = this.state;
    if (loading) {
      return <Loading loading />;
    }
    return <Element currentProjectId={currentProjectId} projects={projects} setCurrentProjectId={this.setCurrentProjectId} newItem={newItem} setCurrentItem={this.setCurrentItem} />;
  }
};
const GanttProjectOrg = () => {
  if (getMenuType() === 'project') {
    return (
      <GanttProject projectId={getProjectId()} />
    );
  }
  return <GanttOrg />;
};
type IReactFCGanttProvideProjects<T = {}> = React.FC<T & { projects: any[], currentProjectId?: string, setCurrentItem:(val:any)=>void, setCurrentProjectId: (val?: string) => void, newItem:any}>
export type { IReactFCGanttProvideProjects };
const GanttOrg = warpGanttProvideProjects(({
  projects, currentProjectId, setCurrentProjectId, newItem, setCurrentItem,
}) => {
  if (projects.length === 0 || !currentProjectId) {
    return (
      <Page>
        <Header>
          <FlatSelect placeholder="项目" />
        </Header>
        <Breadcrumb />
        <Content>
          <EmptyPage image={noDataPic} description="组织下无敏捷项目，请您先创建敏捷项目" />
        </Content>
      </Page>
    );
  }
  return <GanttProject projectId={currentProjectId} newItem={newItem} setCurrentProject={setCurrentProjectId} projects={projects} menuType="org" setCurrentItem={setCurrentItem} />;
});

export default GanttProjectOrg;
