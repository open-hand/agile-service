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
import StartSprint from '../../start-sprint';
import './SprintButton.less';

const { AppState } = stores;

const prefix = 'c7n-backlog-SprintButton';
function SprintButton({
  data,
}) {
  const {
    statusCode, sprintId, belongCurrentPi,
  } = data;
  const issueList = BacklogStore.getIssueListBySprintId(sprintId);
  const hasActiveSprint = BacklogStore.getHasActiveSprint;
  const openStartSprint = async () => {
    if (!BacklogStore.getHasActiveSprint && issueList && issueList.length > 0) {
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
    <div style={{ marginLeft: 'auto' }}>
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
          {(belongCurrentPi === true || belongCurrentPi == undefined) ? (
            <Permission
              service={[IsInProgramStore.isInProgram ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint' : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
            >
              <p
                className={classnames(prefix, {
                  [`${prefix}-disabled`]: hasActiveSprint || !issueList || issueList.length === 0,
                })}
                role="none"
                onClick={openStartSprint}
              >
                开启冲刺
              </p>
            </Permission>
          ) : null}
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
    </div>
  );
}

export default observer(SprintButton);
