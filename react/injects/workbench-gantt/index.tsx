import React, { useState } from 'react';
import { EmptyPage } from '@choerodon/components';
import { find } from 'lodash';

import { useMount } from 'ahooks';
import TableCache from '@/components/table-cache';
import useIsInProgram from '@/hooks/useIsInProgram';
import { ganttApi } from '@/api';
import noDataPic from '@/assets/image/NoData.svg';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import useDefaultMyFilter from '@/hooks/useDefaultMyFilter';
import Gantt from './stores';
import { Loading } from '@/components';
// import WorkbenchGantt from './Gantt';
const WorkbenchGantt: React.FC = () => {
  const [currentProject, setCurrentProject] = useState<any>();
  const [projectLoading, setProjectLoading] = useState(true);
  const [projects, setProjects] = useState<any[]>([]);

  const { isInProgram, loading: programLoading } = useIsInProgram({ projectId: currentProject }, { enabled: !!currentProject });
  const { data: myFilter, isLoading } = useDefaultMyFilter(currentProject, { enabled: !!currentProject });
  const loading = programLoading || isLoading;
  useMount(() => {
    ganttApi.loadProjects().then((res: any) => {
      const cacheProjectId = localPageCacheStore.getItem('org.gantt.projectId');
      const newProjects = res.map((i: any) => ({ ...i, id: String(i.id) }));
      const newProjectId = (find(newProjects, { id: cacheProjectId }) || newProjects[0])?.id;
      localPageCacheStore.setItem('org.gantt.projectId', newProjectId);
      setProjects(newProjects);
      setCurrentProject(newProjectId);
      setProjectLoading(false);
    });
  });
  if (projectLoading) {
    return <Loading loading />;
  }
  return projects.length && currentProject && !loading ? (
    <TableCache type="gantt" projectId={currentProject}>
      {(cacheProps) => (
        <Gantt
          myDefaultFilter={myFilter}
          {...cacheProps}
          isInProgram={isInProgram}
          menuType="org"
          projectId={currentProject}
          projects={projects}
          setCurrentProject={setCurrentProject}
        />
      )}
    </TableCache>
  ) : <EmptyPage image={noDataPic} description="组织下无敏捷项目，请您先创建敏捷项目" />;
};

export default WorkbenchGantt;
