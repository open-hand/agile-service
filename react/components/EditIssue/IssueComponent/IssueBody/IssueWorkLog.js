import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon, Button, Tooltip } from 'choerodon-ui/pro';
import { FormattedMessage } from 'react-intl';
import openRecordWorkLogModal from '@/components/DailyLog/DailyLogPro';
import Log from '../../Component/Log';
import EditIssueContext from '../../stores';
import Divider from './Divider';

const IssueWorkLog = observer(({
  reloadIssue, issueId, onIssueRecordTime,
}) => {
  const { store, disabled, projectId } = useContext(EditIssueContext);

  const renderLogs = () => {
    const worklogs = store.getWorkLogs || [];
    return (
      <div className="c7n-log-list">
        {
          worklogs.map((worklog) => (
            <Log
              key={worklog.logId}
              worklog={worklog}
              onDeleteLog={reloadIssue}
              onUpdateLog={reloadIssue}
              disabled={disabled}
              projectId={projectId}
            />
          ))
        }
      </div>
    );
  };

  return (
    <div id="log">
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          工作日志
        </div>
        {!disabled && (
          <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
            <Tooltip placement="topRight" title="登记工作">
              <Button onClick={() => openRecordWorkLogModal({
                issueId,
                projectId: store.projectId,
                onOk: () => {
                  reloadIssue(issueId);
                  onIssueRecordTime();
                },
              })}
              >
                <Icon type="playlist_add icon" />
              </Button>
            </Tooltip>
          </div>
        )}
      </div>
      {renderLogs()}
      <Divider />
    </div>
  );
});

export default IssueWorkLog;
