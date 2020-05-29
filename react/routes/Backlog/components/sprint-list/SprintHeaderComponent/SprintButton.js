/* eslint-disable eqeqeq */
import React, { Fragment } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon, Dropdown, Menu } from 'choerodon-ui';
import { Permission, stores } from '@choerodon/boot';
import moment from 'moment';
import classnames from 'classnames';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import CloseSprint from '@/components/close-sprint';
import { Tooltip } from 'choerodon-ui/pro/lib';
import StartSprint from '../../start-sprint';
import './SprintButton.less';

const { AppState } = stores;

const prefix = 'c7n-backlog-SprintButton';

function judgeDisabled(arr) {
  for (let i = 0; i < arr.length; i += 1) {
    if (arr[i][0]) {
      return [true, arr[i][1]];
    }
  }
  return [false];
}

function SprintButton({
  data,
}) {
  const {
    statusCode, sprintId, belongCurrentPi,
  } = data;
  const issueList = BacklogStore.getIssueListBySprintId(sprintId);
  const hasActiveSprint = BacklogStore.getHasActiveSprint;
  const [disableStart, reason] = judgeDisabled([[hasActiveSprint, '已有活跃冲刺'], [!issueList || issueList.length === 0, '冲刺下没有问题'], [belongCurrentPi === false, '非当前PI下冲刺不可开启']]);
  const openStartSprint = async () => {
    if (!disableStart) {
      const year = moment().year();
      const workSetting = await BacklogStore.axiosGetWorkSetting(year);
      const sprintDetail = await BacklogStore.axiosGetOpenSprintDetail(data.sprintId);
      StartSprint({
        workSetting,
        sprintDetail,
        data,
      });
    }
  };
  const openCloseSprint = async () => {
    const completeMessage = await BacklogStore.axiosGetSprintCompleteMessage(sprintId);
    CloseSprint({
      completeMessage,
      sprintId,
      afterClose: () => {
        BacklogStore.refresh();
      },
    });
  };
  const menu = (
    <Menu
      onClick={() => { BacklogStore.handleDeleteSprint(data, IsInProgramStore.isShowFeature); }}
    >
      <Menu.Item key="0">
        删除sprint
      </Menu.Item>
    </Menu>
  );

  const { type, id: projectId, organizationId: orgId } = AppState.currentMenuType;
  
  return (
    <Fragment>
      {statusCode === 'started' ? (
        <Permission
          service={[IsInProgramStore.isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
        >
          <p
            className={prefix}
            role="none"
            onClick={openCloseSprint}
          >
            完成冲刺
          </p>
        </Permission>
      ) : (
        <Fragment>
          <Permission
            service={[IsInProgramStore.isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
          >
            <Tooltip title={reason}>
              <p
                className={classnames(prefix, {
                  [`${prefix}-disabled`]: disableStart,
                })}
                role="none"
                onClick={openStartSprint}
              >
                开启冲刺
              </p>
            </Tooltip>
          </Permission>
          <Permission
            type={type}
            projectId={projectId}
            organizationId={orgId}
            service={[IsInProgramStore.isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
          >
            {(data.sprintType !== 'ip'
                && (
                  <Dropdown overlay={menu} trigger={['click']}>
                    <Icon style={{ cursor: 'pointer', marginRight: 15 }} type="more_vert" />
                  </Dropdown>
                )
              )}
          </Permission>
        </Fragment>
      )}
    </Fragment>
  );
}

export default observer(SprintButton);
