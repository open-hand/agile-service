/* eslint-disable react/jsx-curly-brace-presence */
import React, { useState, useContext, useRef } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Button, Icon, Progress, Input, Tooltip,
} from 'choerodon-ui';
import { stores, Choerodon } from '@choerodon/boot';
import { useIssueTypes } from '@/hooks';
import { issueApi, fieldApi } from '@/api';
import { checkCanQuickCreate } from '@/utils/quickCreate';
import CreateSubTask from '../../../CreateIssue/CreateSubTask';
import IssueList from '../../Component/IssueList';
import EditIssueContext from '../../stores';
import './SubTask.less';
import Divider from './Divider';

const { AppState } = stores;

const SubTask = observer(({
  onDeleteSubIssue, reloadIssue, onUpdate, parentSummary,
}) => {
  const creatingRef = useRef(false);
  const { store, disabled } = useContext(EditIssueContext);

  const [expand, setExpand] = useState(false);
  const [summary, setSummary] = useState(false);
  const [issueTypes] = useIssueTypes({ disabled });
  const {
    issueId: parentIssueId, subIssueVOList = [], priorityId, sprintId, typeCode, relateIssueId,
  } = store.getIssue;
  const { getCreateSubTaskShow: createSubTaskShow } = store;
  const disableCreate = disabled || (typeCode === 'bug' && relateIssueId);
  const renderIssueList = (issue, i) => (
    <IssueList
      showAssignee
      showDelete={!disabled}
      showPriority
      key={issue.issueId}
      issue={{
        ...issue,
        typeCode: issue.typeCode || 'sub_task',
      }}
      i={i}
      onOpen={() => {
        if (reloadIssue) {
          reloadIssue(issue.issueId);
        }
      }}
      onRefresh={() => {
        if (reloadIssue) {
          reloadIssue(parentIssueId);
        }
        if (onUpdate) {
          onUpdate();
        }
        if (onDeleteSubIssue) {
          onDeleteSubIssue();
        }
      }}
    />
  );

  const renderSubIssues = () => (
    <div className="c7n-tasks">
      {
        subIssueVOList.map((subIssue, i) => renderIssueList(subIssue, i))
      }
    </div>
  );

  const handleCreateSubIssue = () => {
    store.setCreateSubTaskShow(false);
    if (onUpdate) {
      onUpdate();
    }
    if (reloadIssue) {
      reloadIssue();
    }
  };

  const getPercent = () => {
    const completeList = subIssueVOList.filter((issue) => issue.completed);
    const allLength = (subIssueVOList && subIssueVOList.length) || 0;
    const completeLength = completeList.length;
    if (allLength === 0) {
      return 100;
    }
    return parseInt(completeLength / allLength * 100, 10);
  };

  const handleCancel = () => {
    setExpand(false);
    setSummary(false);
  };

  const handleSave = async () => {
    if (creatingRef.current) {
      return;
    }
    const subIssueType = issueTypes.find((t) => t.typeCode === 'sub_task');
    if (summary) {
      creatingRef.current = true;
      if (!await checkCanQuickCreate(subIssueType.typeCode)) {
        Choerodon.prompt('该问题类型含有必填选项，请使用弹框创建');
        creatingRef.current = false;
        return;
      }
      const issue = {
        summary,
        priorityId,
        priorityCode: `priority-${priorityId}`,
        projectId: AppState.currentMenuType.id,
        parentIssueId,
        sprintId,
        issueTypeId: subIssueType && subIssueType.id,
      };
      issueApi.createSubtask(issue)
        .then((res) => {
          const dto = {
            schemeCode: 'agile_issue',
            context: subIssueType && subIssueType.typeCode,
            pageCode: 'agile_issue_create',
          };
          fieldApi.quickCreateDefault(res.issueId, dto);
          handleCancel();
          handleCreateSubIssue();
        })
        .catch(() => {
        }).finally(() => {
          creatingRef.current = false;
        });
    } else {
      Choerodon.prompt('子任务概要不能为空！');
    }
  };

  const onSummaryChange = (e) => {
    setSummary(e.target && e.target.value && e.target.value.trim());
  };
  return (
    disableCreate && subIssueVOList.length === 0 ? null : (
      <div id="sub_task">
        <Divider />
        <div className="c7n-title-wrapper">
          <div className="c7n-title-left">
            <span>子任务</span>
          </div>
          {!disableCreate && (
          <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
            <Tooltip placement="topRight" title="创建子任务" getPopupContainer={(triggerNode) => triggerNode.parentNode}>
              <Button style={{ padding: '0 6px' }} className="leftBtn" funcType="flat" onClick={() => store.setCreateSubTaskShow(true)}>
                <Icon type="playlist_add icon" />
              </Button>
            </Tooltip>
          </div>
          )}
        </div>
        {subIssueVOList && subIssueVOList.length
          ? (
            <div className="c7n-subTask-progress">
              <Progress percent={getPercent()} style={{ marginRight: 5 }} />
              已完成
            </div>
          ) : ''}
        {renderSubIssues()}
        {!disableCreate && (
        <div className="c7n-subTask-quickCreate">
          {expand
            ? (
              <div style={{ display: 'flex', alignItems: 'center' }}>
                <Input
                  autoFocus
                  className="hidden-label"
                  placeholder="在此输入子任务概要"
                  maxLength={44}
                  onPressEnter={handleSave}
                  onChange={onSummaryChange}
                />
                <Button
                  type="primary"
                  funcType="raised"
                  style={{ margin: '0 10px' }}
                  onClick={handleSave}
                >
                  确定
                </Button>
                <Button
                  funcType="raised"
                  onClick={handleCancel}
                >
                  取消
                </Button>
              </div>
            ) : (
              <Button
                className="leftBtn"
                functyp="flat"
                onClick={() => {
                  setExpand(true);
                }}
              >
                <Icon type="playlist_add" />
                {'快速创建子任务'}
              </Button>
            )}
        </div>
        )}
        {
        createSubTaskShow ? (
          <CreateSubTask
            parentIssueId={parentIssueId}
            parentSummary={parentSummary}
            visible={createSubTaskShow}
            onCancel={() => store.setCreateSubTaskShow(false)}
            onOk={handleCreateSubIssue}
            store={store}
          />
        ) : null
      }
      </div>
    )
  );
});

export default SubTask;
