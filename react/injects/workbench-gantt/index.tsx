import React from 'react';
import { EmptyPage } from '@choerodon/components';

import TableCache from '@/components/table-cache';
import useIsInProgram from '@/hooks/useIsInProgram';
import noDataPic from '@/assets/image/NoData.svg';
import useDefaultMyFilter from '@/hooks/useDefaultMyFilter';
import Gantt, { StoreProvider } from './stores';
import type { IReactFCGanttProvideProjects } from '@/routes/gantt';
import { warpGanttProvideProjects } from '@/routes/gantt';

const WorkbenchProjectGantt: IReactFCGanttProvideProjects = ({ projects, currentProjectId, setCurrentProjectId }) => {
  const { isInProgram, loading: programLoading } = useIsInProgram({ projectId: currentProjectId });

  const { data: myFilter, isLoading } = useDefaultMyFilter({ projectId: currentProjectId, menuType: 'project' });
  const loading = programLoading || isLoading;
  if (projects.length === 0) {
    return <EmptyPage image={noDataPic} description="组织下无敏捷项目，请您先创建敏捷项目" />;
  }
  return !loading ? (
    <TableCache type="gantt" projectId={currentProjectId}>
      {(cacheProps) => (
        <Gantt
          myDefaultFilter={myFilter}
          {...cacheProps}
          isInProgram={isInProgram}
          menuType="org"
          projectId={currentProjectId}
          projects={projects}
          setCurrentProject={setCurrentProjectId}
        />
      )}
    </TableCache>
  ) : null;
};

const WorkbenchGantt = (props: any) => (
  <StoreProvider {...props}>
    <WorkbenchProjectGantt {...props} />
  </StoreProvider>
);

export default warpGanttProvideProjects(WorkbenchGantt, 'workbench');
