import React from 'react';
import {
  HeaderButtons,
} from '@choerodon/boot';
import { LoadingProvider } from '@choerodon/components';
import { useMount } from 'ahooks';
import classNames from 'classnames';
import SelectProject from '@/components/select/select-project';
import StatusLinkageWSHandle from '@/components/StatusLinkageWSHandle';
import styles from './Gantt.less';
import { useGanttContext } from '@/routes/gantt/stores/context';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import useGanttHeader from '@/routes/gantt/hooks/useGanttHeader';
import SelectSprint from '@/components/select/select-sprint';
import SelectType from '@/routes/gantt/components/gannt-select/SelectType';
import SelectProcessType from '@/routes/gantt/components/gannt-select/SelectProcessType';
import GanttBody from '@/routes/gantt/GanttBody';

const WorkbenchGantt: React.FC = () => {
  const {
    menuType, projectId, projects, setCurrentProject, store, issueSearchStore,
  } = useGanttContext();
  const [ganttHeaderData, {
    typeComponentProps, sprintComponentsProps, processTypeComponentProps, headerComponentProps,
  }] = useGanttHeader({
    projectId,
    store,
    menuType: 'workbench',
  });
  useMount(() => {
    // ganttHeaderData.fullDomRef.current = document.getElementsByClassName(styles.page)[0] as any;
  });
  return (
    <LoadingProvider className={classNames(styles.page, { [styles.full]: ganttHeaderData.isFullScreen })}>
      <div className={styles.header}>
        <div className={styles.header_left}>
          <SelectProject
            value={projectId}
            flat
            clearButton={false}
            style={{ marginRight: 16 }}
            maxTagTextLength={12}
            placeholder="项目"
            optionData={projects}
            onChange={(val) => {
              setCurrentProject && setCurrentProject((oldValue: string) => {
                if (oldValue === val || !val) {
                  return String(oldValue);
                }
                store.setSprintIds(null);
                localPageCacheStore.setItem('org.gantt.projectId', val);
                return val;
              });
            }}
          />
          <SelectSprint {...sprintComponentsProps} />
          <SelectType {...typeComponentProps} />
          <SelectProcessType {...processTypeComponentProps} />
        </div>
        <HeaderButtons {...headerComponentProps} />

      </div>
      <div className={styles.content}>
        <GanttBody {...ganttHeaderData} />
      </div>
      <StatusLinkageWSHandle />
    </LoadingProvider>
  );
};

export default WorkbenchGantt;
