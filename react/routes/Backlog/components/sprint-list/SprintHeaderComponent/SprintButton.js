import React, { Fragment } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon, Dropdown, Menu } from 'choerodon-ui';
import moment from 'moment';
import classnames from 'classnames';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import StartSprint from '../../start-sprint';
import CloseSprint from '../../close-sprint';
import './SprintButton.less';

const prefix = 'c7n-backlog-SprintButton';
function SprintButton({
  data,
}) {
  const {
    statusCode, sprintId, piId, 
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
      data,
    });
  };
  const menu = (
    <Menu
      onClick={() => { BacklogStore.handleDeleteSprint(data); }}
    >
      <Menu.Item key="0">
        删除sprint
      </Menu.Item>
    </Menu>
  );
  return statusCode === 'started' ? (
    <p
      className={prefix}
      role="none"
      onClick={openCloseSprint}
    >
      完成冲刺
    </p>
  ) : (
    <Fragment>
      <p
        className={classnames(prefix, {
          [`${prefix}-disabled`]: hasActiveSprint || !issueList || issueList.length === 0,
        })}
        role="none"
        onClick={openStartSprint}
      >
        开启冲刺
      </p>
      {piId
        ? '' : (
          <Dropdown overlay={menu} trigger={['click']}>
            <Icon style={{ cursor: 'pointer', marginRight: 15 }} type="more_vert" />
          </Dropdown>
        )
          }
    </Fragment>
  );
}

export default observer(SprintButton);
