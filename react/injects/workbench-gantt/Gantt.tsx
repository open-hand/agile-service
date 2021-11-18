import React, { useState } from 'react';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
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
    menuType,
  });
  const [loading, setLoading] = useState(false);
  return (
    <div className={styles.page}>
      <div className={styles.header}>
        <div className={styles.header_left}>
          <SelectProject
            value={projectId}
            flat
            clearButton={false}
            style={{ marginRight: 16 }}
            maxTagTextLength={12}
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
    </div>
  );
};

export default WorkbenchGantt;
