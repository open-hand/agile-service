import React from 'react';
import { Select } from 'choerodon-ui/pro';
import styles from './index.less';
import { IMode, useIssueStore } from '../../stores';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

const { Option } = Select;
const ModeSwitch = () => {
  const { isProject, setMode, mode } = useIssueStore();
  const handleChange = (value: IMode) => {
    setMode(value);
    localPageCacheStore.setItem('workingHours-issue-mode', value);
  };
  return (
    <div className={styles.switch}>
      <Select onChange={handleChange} value={mode} clearButton={false}>
        {
          isProject ? (
            <>
              <Option value="issue" key="issue">按工作项查看</Option>
              <Option value="assignee" key="assignee">按经办人查看</Option>
            </>
          ) : (
            <>
              <Option value="project" key="list">按项目查看</Option>
              <Option value="assignee" key="tree">按经办人查看</Option>
              <Option value="projectAssignee" key="tree">按项目/经办人查看</Option>
            </>
          )
        }
      </Select>
    </div>
  );
};
export default ModeSwitch;
