import React from 'react';
import { Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import SprintName from './SprintHeaderComponent/SprintName';
import SprintStatus from './SprintHeaderComponent/SprintStatus';
import SprintButton from './SprintHeaderComponent/SprintButton';
import SprintVisibleIssue from './SprintHeaderComponent/SprintVisibleIssue';
import AssigneeInfo from './SprintHeaderComponent/AssigneeInfo';
import StoryPoint from './SprintHeaderComponent/StoryPoint';
import SprintDateRange from './SprintHeaderComponent/SprintDateRange';
import SprintGoal from './SprintHeaderComponent/SprintGoal';
import WorkLoadBtn from './SprintHeaderComponent/WorkLoadBtn';
import SprintIcon from './SprintHeaderComponent/SprintIcon';
import './SprintHeader.less';

const prefix = 'c7n-backlog-SprintHeader';
function BacklogHeader({ data }) {
  const {
    expand, sprintId,
  } = data;
  return (
    <div className={prefix}>
      <div className={`${prefix}-backlog`}>
        <Icon
          style={{ fontSize: 20, cursor: 'pointer' }}
          type={expand ? 'baseline-arrow_drop_down' : 'baseline-arrow_right'}
          role="none"
          onClick={() => { BacklogStore.expandSprint(sprintId, !expand); }}
        />
        <SprintName data={data} />
        <SprintVisibleIssue
          data={data}
        />
      </div>
    </div>
  );
}
function SprintHeader({ data }) {
  const {
    type, expand, sprintId, piId, sprintType,
  } = data;
  const isSprint = type === 'sprint';
  return (
    isSprint ? (
      <div className={prefix}>
        <div className={`${prefix}-top`}>
          <Icon
            style={{ fontSize: 20, cursor: 'pointer' }}
            type={expand ? 'baseline-arrow_drop_down' : 'baseline-arrow_right'}
            role="none"
            onClick={() => { BacklogStore.expandSprint(sprintId, !expand); }}
          />
          <SprintIcon sprintType={sprintType} />
          <SprintName data={data} />
          <SprintVisibleIssue
            data={data}
          />
          <SprintStatus
            data={data}
          />
          <WorkLoadBtn data={data} />
          <SprintButton
            data={data}
          />
        </div>
        <div className={`${prefix}-middle`}>
          <AssigneeInfo
            data={data}
          />
          <StoryPoint
            data={data}
          />
        </div>
        <div className={`${prefix}-bottom`}>
          <SprintDateRange            
            data={data}
          />
          <SprintGoal
            data={data}
          />
        </div>
      </div>
    ) : <BacklogHeader data={data} />
  );
}

export default observer(SprintHeader);
