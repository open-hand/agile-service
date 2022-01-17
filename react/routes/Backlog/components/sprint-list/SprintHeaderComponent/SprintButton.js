/* eslint-disable eqeqeq */
import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon, Dropdown, Menu } from 'choerodon-ui';
import { Permission, stores } from '@choerodon/boot';
import moment from 'moment';
import classnames from 'classnames';
import { Tooltip, Button } from 'choerodon-ui/pro';
import { sprintApi, workCalendarApi } from '@/api';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import CloseSprint from '@/components/close-sprint';
import StartSprint from '../../start-sprint';
import './SprintButton.less';
import useFormatMessage from '@/hooks/useFormatMessage';

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

function SprintButton({ data, sprintIndex }) {
  const {
    statusCode, sprintId, planning, issueCount,
  } = data;
  const formatMessage = useFormatMessage();
  const hasActiveSprint = BacklogStore.getHasActiveSprint;

  const [disableStart, reason] = judgeDisabled([
    [hasActiveSprint, '已有活跃冲刺'],
    [issueCount <= 0, '冲刺下没有工作项'],
    [planning === true, '非当前PI下冲刺不可开启'],
  ]);

  const { isShowFeature, isInProgram } = BacklogStore.getIsInProgramData || {};

  const openStartSprint = async () => {
    setCreateBtnToolTipHidden(true);
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
    const defaultValuePrompt = undefined; // await sprintApi.beforeChangeCheck(sprintId) ? `冲刺${data.sprintName}是默认选项，完成后冲刺字段默认值将清空` : undefined;
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
      onClick={() => {
        BacklogStore.handleDeleteSprint(data, isShowFeature);
      }}
    >
      <Menu.Item key="0">
        {formatMessage({ id: 'agile.backlog.delete.sprint' })}
      </Menu.Item>
    </Menu>
  );

  const [createBtnToolTipHidden, setCreateBtnToolTipHidden] = useState(true);
  const [inNewUserGuideStepThree, setInNewUserGuideStepThree] = useState(false);

  useEffect(() => {
    if (
       AppState?.getUserWizardStatus[2]?.status === 'uncompleted'
    ) {
      setInNewUserGuideStepThree(true);
      setCreateBtnToolTipHidden(false);
      if (issueCount === 0) { // 还没建工作项之前
        setCreateBtnToolTipHidden(true);
      }
    } else {
      setInNewUserGuideStepThree(false);
      setCreateBtnToolTipHidden(true);
    }
  }, [AppState.getUserWizardStatus]);

  useEffect(() => {
    if (inNewUserGuideStepThree && issueCount === 1) { // 第二步建了工作项
      setCreateBtnToolTipHidden(false);
    }
  }, [issueCount]);

  const toHelpDoc = () => {
    window.open(
      `${AppState?.getUserWizardStatus[2]?.helpDocs[2]}`,
      '_blank',
    );
  };

  const onHiddenBeforeChange = (hidden) => {
    if (reason) {
      setCreateBtnToolTipHidden(hidden);
    }
    if (inNewUserGuideStepThree && createBtnToolTipHidden === true && !hidden) {
      setCreateBtnToolTipHidden(hidden);
    }
  };

  const getCreatBtnTitle = () => {
    if (reason) {
      return reason;
    }
    if (inNewUserGuideStepThree) {
      return (
        <div style={{ background: '#6E80F1 !important' }}>
          <div style={{ padding: 8 }}>
            在开启冲刺前，规划冲刺一起对任务需求进行定义和评估。您可以对一个工作项进行拆分，创建其子任务、给一个问题设置故事点（预估工作量）、预估问题的完成时间、设置问题报告人和经办人等，完成工作量评估即可开启冲刺。
          </div>
          <div style={{ textAlign: 'right' }}>
            <Button
              onClick={() => {
                setCreateBtnToolTipHidden(true);
              }}
              style={{ color: '#fff', background: '#7E90F1' }}
            >
              忽略
            </Button>
            <Button
              onClick={toHelpDoc}
              style={{ color: '#5365EA', background: '#fff' }}
            >
              查看
            </Button>
          </div>
        </div>
      );
    }
    return '';
  };

  const getHidden = () => {
    if (reason) {
      return createBtnToolTipHidden;
    }
    if (sprintIndex === 0 && !reason && inNewUserGuideStepThree && issueCount > 0) { // 第一个冲刺项目并且是新手指引状态并且已经有工作项
      return createBtnToolTipHidden;
    }
    return true;
  };

  const {
    type,
    id: projectId,
    organizationId: orgId,
  } = AppState.currentMenuType;

  return (
    <>
      {statusCode === 'started' ? (
        <Permission
          service={[
            isInProgram
              ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint'
              : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint',
          ]}
        >
          <p
            className={prefix}
            role="none"
            onClick={openCloseSprint}
          >
            {formatMessage({ id: 'agile.common.complete.sprint' })}
          </p>
        </Permission>
      ) : (
        <>
          <Permission
            service={[
              isInProgram
                ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint'
                : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint',
            ]}
          >
            <Tooltip
              popupClassName={
                inNewUserGuideStepThree && issueCount > 0 ? 'c7n-pro-popup-open-sprint-guide' : ''
              }
              hidden={getHidden()}
              onHiddenBeforeChange={onHiddenBeforeChange}
              title={getCreatBtnTitle}
              placement={inNewUserGuideStepThree && issueCount > 0 ? 'bottomLeft' : 'bottom'}
            >
              <p
                className={classnames(prefix, {
                  [`${prefix}-disabled`]: disableStart,
                })}
                role="none"
                onClick={openStartSprint}
              >
                {formatMessage({ id: 'agile.backlog.start.sprint' })}
              </p>
            </Tooltip>
          </Permission>
          <Permission
            type={type}
            projectId={projectId}
            organizationId={orgId}
            service={[
              isInProgram
                ? 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint'
                : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint',
            ]}
          >
            {data.sprintType !== 'ip' && (
              <Dropdown overlay={menu} trigger={['click']}>
                <Icon
                  style={{ cursor: 'pointer', marginRight: 15 }}
                  type="more_vert"
                />
              </Dropdown>
            )}
          </Permission>
        </>
      )}
    </>
  );
}

export default observer(SprintButton);
