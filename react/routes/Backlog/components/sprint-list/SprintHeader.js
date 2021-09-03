import React from 'react';
import { Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { Permission } from '@choerodon/boot';
import useIsInProgram from '@/hooks/useIsInProgram';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import SprintStatus from '@/components/tag/sprint-status-tag/SprintStatus';
import SprintStoryPointInfo from '@/components/SprintStoryPointInfo';
import SprintName from './SprintHeaderComponent/SprintName';
import SprintButton from './SprintHeaderComponent/SprintButton';
import SprintVisibleIssue from './SprintHeaderComponent/SprintVisibleIssue';
import AssigneeInfo from './SprintHeaderComponent/AssigneeInfo';
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
  const { isInProgram } = BacklogStore.getIsInProgramData || {}
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
          <Permission
            service={[isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
            noAccessChildren={(
              <SprintName
                data={data}
                noPermission
              />
            )}
          >
            <SprintName data={data} />
          </Permission>
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
          <SprintStoryPointInfo
            show={data.statusCode === 'started'}
            data={data}
          />
        </div>
        <div className={`${prefix}-bottom`}>
          <Permission
            service={[isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
            noAccessChildren={(
              <SprintDateRange
                data={data}
                noPermission
              />
            )}
          >
            <SprintDateRange
              data={data}
            />
          </Permission>
          <Permission
            service={[isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
            noAccessChildren={(
              <SprintGoal
                data={data}
                noPermission
              />
            )}
          >
            <SprintGoal
              data={data}
            />
          </Permission>
        </div>
      </div>
    ) : <BacklogHeader data={data} />
  );
}

export default observer(SprintHeader);
