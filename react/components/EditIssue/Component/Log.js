/* eslint-disable react/jsx-no-bind */
import React, { Component } from 'react';
import { Icon, Popconfirm } from 'choerodon-ui';
import { workLogApi } from '@/api';
import UserTag from '@/components/tag/user-tag';

import WYSIWYGEditor from '../../CKEditor';
import WYSIWYGViewer from '../../CKEditorViewer';
import './Log.less';

class Log extends Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      editLogId: undefined,
      editLog: undefined,
      expand: true,
    };
  }

  updateLog = (log) => {
    const { onUpdateLog, projectId } = this.props;
    workLogApi.project(projectId).update(log.logId, log).then((res) => {
      this.setState({
        editLogId: undefined,
        editLog: undefined,
      });
      onUpdateLog();
    });
  };

  handleDeleteLog(logId) {
    const { onDeleteLog, projectId } = this.props;
    workLogApi.project(projectId).delete(logId)
      .then((res) => {
        onDeleteLog();
      });
  }

  async handleUpdateLog(log) {
    const { logId, objectVersionNumber } = log;
    const { editLog } = this.state;
    try {
      const text = editLog;
      this.updateLog({
        logId,
        objectVersionNumber,
        description: text,
      });
    } catch (error) {
      //
    }
  }

  cancel(e) {
  }

  confirm(logId, e) {
    this.handleDeleteLog(logId);
  }

  render() {
    const {
      worklog, isWide, disabled,
    } = this.props;
    const { editLog, editLogId, expand } = this.state;
    const {
      realName, loginName,
      userName, createdBy, userImageUrl,
    } = worklog;
    const deltaEdit = editLog;

    return (
      <div
        className={`c7n-log ${worklog.logId === editLogId ? 'c7n-log-focus' : ''}`}
      >
        <div className="line-justify">
          {
            expand ? (
              <Icon
                role="none"
                style={{
                  position: 'absolute',
                  left: 5,
                  top: 13,
                }}
                type="baseline-arrow_drop_down pointer"
                onClick={() => {
                  this.setState({
                    expand: false,
                  });
                }}
              />
            ) : null
          }
          {
            !expand ? (
              <Icon
                role="none"
                style={{
                  position: 'absolute',
                  left: 5,
                  top: 15,
                }}
                type="baseline-arrow_right pointer"
                onClick={() => {
                  this.setState({
                    expand: true,
                  });
                }}
              />
            ) : null
          }
          <div className="c7n-title-log">
            <div style={{ marginRight: 19 }}>
              <UserTag
                data={{
                  id: createdBy,
                  realName,
                  loginName,
                  imageUrl: userImageUrl,
                  tooltip: userName,
                }}
                textStyle={{ color: '#5365EA' }}
              />
            </div>
            <span style={{ color: 'var(--text-color3)', marginLeft: 15 }}>
              {worklog.lastUpdateDate}
            </span>
          </div>
          <div className="c7n-action">
            {
              !disabled && (
              <Icon
                role="none"
                type="edit-o"
                style={{ marginRight: 12 }}
                onClick={() => {
                  this.setState({
                    editLogId: worklog.logId,
                    editLog: worklog.description,
                    expand: true,
                  });
                }}
              />
              )
            }

            {!disabled
              ? (
                <Popconfirm
                  title="确认要删除该工作日志吗?"
                  placement="left"
                  onConfirm={this.confirm.bind(this, worklog.logId)}
                  onCancel={this.cancel}
                  okText="删除"
                  cancelText="取消"
                  okType="danger"
                >
                  <Icon
                    type="delete_sweep-o"
                  />
                </Popconfirm>
              ) : ''}
          </div>
        </div>
        <div className="line-start" style={{ color: 'var(--text-color3)', marginTop: '10px' }}>
          <span style={{ width: 70 }}>耗费时间:</span>
          <span style={{ color: 'var(--text-color)', fontWeight: '500' }}>{`${worklog.workTime}小时` || '无'}</span>
        </div>
        <div className="line-start" style={{ color: 'var(--text-color3)', marginTop: '10px' }}>
          <span style={{ width: 70 }}>工作日期:</span>
          <span style={{ color: 'var(--text-color)', fontWeight: '500' }}>{worklog.startDate || '无'}</span>
        </div>
        {
          expand && (
            <div>
              <div className="c7n-conent-log" style={{ marginTop: 10, display: 'flex' }}>
                <span style={{ width: 70, flexShrink: 0, color: 'var(--text-color3)' }}>备注:</span>
                <span style={{ flex: 1 }}>
                  {
                    worklog.logId !== editLogId ? (
                      <WYSIWYGViewer value={worklog.description} />
                    ) : null
                  }
                </span>
              </div>
              {
                worklog.logId === editLogId ? (
                  <WYSIWYGEditor
                    autoFocus
                    footer
                    value={deltaEdit}
                    style={{ minHeight: 300, width: '100%' }}
                    onChange={(value) => {
                      this.setState({ editLog: value });
                    }}
                    onCancel={() => {
                      this.setState({
                        editLogId: undefined,
                        editLog: undefined,
                      });
                    }}
                    onOk={this.handleUpdateLog.bind(this, worklog)}
                    toolbarHeight={isWide ? null : 66}
                  />
                ) : null
              }
            </div>
          )
        }

      </div>
    );
  }
}

export default Log;
