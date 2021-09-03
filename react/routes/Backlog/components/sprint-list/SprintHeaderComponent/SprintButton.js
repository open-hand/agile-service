/* eslint-disable eqeqeq */
import React, { Fragment } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Icon, Dropdown, Menu,
} from 'choerodon-ui';
import { Permission, stores } from '@choerodon/boot';
import moment from 'moment';
import classnames from 'classnames';
import { sprintApi, workCalendarApi } from '@/api';
import useIsInProgram from '@/hooks/useIsInProgram';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import CloseSprint from '@/components/close-sprint';
import { Tooltip } from 'choerodon-ui/pro';
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
    statusCode, sprintId, planning, issueCount,
  } = data;
  const hasActiveSprint = BacklogStore.getHasActiveSprint;
  const [disableStart, reason] = judgeDisabled([[hasActiveSprint, '已有活跃冲刺'], [issueCount <= 0, '冲刺下没有问题'], [planning === true, '非当前PI下冲刺不可开启']]);
  const { isShowFeature, isInProgram } = BacklogStore.getIsInProgramData || {};
  const openStartSprint = async () => {
    if (!disableStart) {
      const year = moment().year();
      const workSetting = await workCalendarApi.getWorkSetting(year);
      const sprintDetail = await sprintApi.loadSprint(data.sprintId);
      StartSprint({
        workSetting,
        sprintDetail,
        data,
      });
    }
  };
  const openCloseSprint = async () => {
    const completeMessage = await sprintApi.loadSprintAndCountIssue(sprintId);
    const defaultValuePrompt = undefined;// await sprintApi.beforeChangeCheck(sprintId) ? `冲刺${data.sprintName}是默认选项，完成后冲刺字段默认值将清空` : undefined;
    CloseSprint({
      completeMessage,
      defaultValuePrompt,
      sprintId,
      afterClose: () => {
        BacklogStore.refresh();
      },
    });
  };
  const menu = (
    <Menu
      onClick={() => { BacklogStore.handleDeleteSprint(data, isShowFeature); }}
    >
      <Menu.Item key="0">
        删除sprint
      </Menu.Item>
    </Menu>
  );

  const { type, id: projectId, organizationId: orgId } = AppState.currentMenuType;

  return (
    <>
      {statusCode === 'started' ? (
        <Permission
          service={[isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
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
        <>
          <Permission
            service={[isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
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
            service={[isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
          >
            {(data.sprintType !== 'ip'
                && (
                  <Dropdown overlay={menu} trigger={['click']}>
                    <Icon style={{ cursor: 'pointer', marginRight: 15 }} type="more_vert" />
                  </Dropdown>
                )
              )}
          </Permission>
        </>
      )}
    </>
  );
}

export default observer(SprintButton);
