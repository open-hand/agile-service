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
import Gantt from './Gantt';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import useDefaultMyFilter from '@/hooks/useDefaultMyFilter';

// eslint-disable-next-line react/require-default-props
const GanttProject: React.FC<{ projectId: string, menuType?: 'project' | 'org', HeadSelect?: JSX.Element, [key: string]: any }> = ({
  projectId, projects, setCurrentProject, menuType = 'project',
}) => {
  const { isInProgram, loading: programLoading } = useIsInProgram();
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

const GanttProjectOrg = () => {
  if (getMenuType() === 'project') {
    return (
      <GanttProject projectId={getProjectId()} />
    );
  }
  return <GanttOrg />;
};
export default GanttProjectOrg;
