import React, { useState, useEffect } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { EmptyPage, FlatSelect } from '@choerodon/components';
import { find } from 'lodash';

import TableCache from '@/components/table-cache';
import useIsInProgram from '@/hooks/useIsInProgram';
import { getMenuType, getProjectId } from '@/utils/common';
import { ganttApi } from '@/api';
import noDataPic from '@/assets/image/NoData.svg';
import Gantt from './stores';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import useDefaultMyFilter from '@/hooks/useDefaultMyFilter';
import { Loading } from '@/components';

// eslint-disable-next-line react/require-default-props
const GanttProject: React.FC<{ projectId: string, menuType?: 'project' | 'org', HeadSelect?: JSX.Element, [key: string]: any }> = ({
  projectId, projects, setCurrentProject, menuType = 'project',
}) => {
  const { isInProgram, loading: programLoading } = useIsInProgram({ projectId });
  const { data: myFilter, isLoading } = useDefaultMyFilter(projectId);
  const loading = programLoading || isLoading;
  if (menuType === 'project') {
    return !loading ? (
      <TableCache type="gantt" projectId={projectId}>
        {(cacheProps) => <Gantt myDefaultFilter={myFilter} {...cacheProps} isInProgram={isInProgram} menuType="project" projectId={projectId} />}
      </TableCache>
    ) : <span />;
  }
  return projects.length && projectId && !loading ? (
    <TableCache type="gantt" projectId={projectId}>
      {(cacheProps) => (
        <Gantt myDefaultFilter={myFilter} {...cacheProps} isInProgram={isInProgram} menuType="org" projectId={projectId} projects={projects} setCurrentProject={setCurrentProject} />
      )}
    </TableCache>
  ) : null;
};
const GanttOrg = () => {
  const [currentProject, setCurrentProject] = useState<any>();
  const [projectIds, setProjectIds] = useState<any[]>([]);
  const [loading, setLoading] = useState(true);
  useEffect(() => {
    ganttApi.loadProjects().then((res: any) => {
      const cacheProjectId = localPageCacheStore.getItem('org.gantt.projectId');
      const newProjects = res.map((i: any) => ({ ...i, id: String(i.id) }));
      const newProjectId = (find(newProjects, { id: cacheProjectId }) || newProjects[0])?.id;
      localPageCacheStore.setItem('org.gantt.projectId', newProjectId);
      setProjectIds(newProjects);
      setCurrentProject(newProjectId);
      setLoading(false);
    });
  }, []);
  if (!loading && projectIds.length === 0) {
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
  return <GanttProject projectId={currentProject} setCurrentProject={setCurrentProject} projects={projectIds} menuType="org" projectIds={projectIds} />;
};
/**
 * 高阶包裹  当项目加载好  再去装载 Element
 * @param Element
 */
export function warpGanttProvideProjects(Element: React.FC<any>) {
  class GanttProjectsProvider extends React.PureComponent<any, { projects: any[], currentProjectId?: string, loading: boolean }> {
    constructor(props: any) {
      super(props);
      this.state = {
        projects: [],
        currentProjectId: undefined,
        loading: true,
      };
    }

    componentDidMount() {
      ganttApi.loadProjects().then((res: any) => {
        const cacheProjectId = localPageCacheStore.getItem('org.gantt.projectId');
        const newProjects = res.map((i: any) => ({ ...i, id: String(i.id) }));
        const newProjectId = (find(newProjects, { id: cacheProjectId }) || newProjects[0])?.id;
        localPageCacheStore.setItem('org.gantt.projectId', newProjectId);
        this.setState({
          projects: newProjects,
          currentProjectId: newProjectId,
          loading: false,
        });
      });
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

    render() {
      const { loading, currentProjectId, projects } = this.state;
      if (loading) {
        return <Loading loading />;
      }
      return <Element currentProjectId={currentProjectId} projects={projects} setCurrentProjectId={this.setCurrentProjectId} />;
    }
  }
  return GanttProjectsProvider;
}
const GanttProjectOrg = () => {
  if (getMenuType() === 'project') {
    return (
      <GanttProject projectId={getProjectId()} />
    );
  }
  return <GanttOrg />;
};
type IReactFCGanttProvideProjects<T = {}> = React.FC<T & { projects: any[], currentProjectId?: string, setCurrentProjectId: (val?: string) => void }>
export type { IReactFCGanttProvideProjects };
export default GanttProjectOrg;
