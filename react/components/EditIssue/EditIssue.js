/* eslint-disable react/sort-comp */
import React, {
  useContext, useState, useEffect, useImperativeHandle, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { stores, Choerodon } from '@choerodon/boot';
import { Spin } from 'choerodon-ui';
import { throttle } from 'lodash';
import './EditIssue.less';
import useIsOwner from '@/hooks/useIsOwner';
import { useIssueTypes } from '@/hooks';
import {
  issueApi, fieldApi, issueLinkApi, workLogApi, knowledgeApi, dataLogApi, devOpsApi,
} from '@/api';
import RelateStory from '../RelateStory';
import CopyIssue from '../CopyIssue';
import ResizeAble from '../ResizeAble';
import TransformSubIssue from '../TransformSubIssue';
import TransformFromSubIssue from '../TransformFromSubIssue';
import ChangeParent from '../ChangeParent';
import IssueHeader from './IssueComponent/IssueHeader';
import IssueBody from './IssueComponent/IssueBody/IssueBody';
import EditIssueContext from './stores';
import IsInProgramStore from '../../stores/common/program/IsInProgramStore';
// 项目加入群之后，不关联自己的史诗和特性，只能关联项目群的，不能改关联的史诗
const { AppState } = stores;

const defaultProps = {
  applyType: 'agile',
};

function EditIssue() {
  const [issueLoading, setIssueLoading] = useState(false);
  const {
    afterIssueUpdate,
    store,
    forwardedRef,
    issueId: currentIssueId,
    applyType,
    programId,
    onUpdate,
    onIssueCopy,
    backUrl,
    onCancel,
    style,
    onDeleteIssue,
    onDeleteSubIssue,
    disabled,
    prefixCls,
    FieldVersionRef,
    FieldFixVersionRef,
    intlPrefix,
    issueStore,
    HeaderStore,
    isFullScreen,
    onChangeWidth,
  } = useContext(EditIssueContext);
  const [isOwner] = useIsOwner();
  const [issueTypes] = useIssueTypes();
  const container = useRef();
  const idRef = useRef();
  const loadIssueDetail = async (paramIssueId) => {
    const id = paramIssueId || currentIssueId;
    idRef.current = id;
    setIssueLoading(true);
    try {
      // 1. 加载详情
      const issue = await (programId ? issueApi.loadUnderProgram(id, programId) : issueApi.load(id));
      if (idRef.current !== id) {
        return;
      }
      // 详情中更新信息时，外部列表根据这个新的issue信息进行本地更新
      if (afterIssueUpdate) {
        afterIssueUpdate(issue);
      }
      // 2. 根据详情加载fields
      const param = {
        schemeCode: 'agile_issue',
        context: issue.typeCode,
        pageCode: 'agile_issue_edit',
      };
      const fields = await fieldApi.getFieldAndValue(id, param);
      setIssueLoading(false);
      store.setIssueFields(issue, fields);
      if (issueStore) {
        issueStore.setSelectedIssue(issue);
      }
      // 3. 加载额外信息
      const [
        doc,
        workLogs,
        dataLogs,
        linkIssues,
        branches,
      ] = await Promise.all([
        knowledgeApi.loadByIssue(id),
        programId || applyType === 'program' ? null : workLogApi.loadByIssue(id),
        programId ? dataLogApi.loadUnderProgram(id, programId) : dataLogApi.loadByIssue(id),
        programId || applyType === 'program' ? null : issueLinkApi.loadByIssueAndApplyType(id),
        programId || applyType === 'program' ? null : devOpsApi.countBranches(id),
      ]);
      if (idRef.current !== id) {
        return;
      }
      store.initIssueAttribute(doc, workLogs, dataLogs, linkIssues, branches, []);
    } catch (error) {
      Choerodon.prompt(error.message, 'error');
    }
  };

  const setQuery = (width = container.current.clientWidth) => {
    if (width <= 600) {
      container.current.setAttribute('max-width', '600px');
    } else {
      container.current.removeAttribute('max-width');
    }
  };

  useEffect(() => {
    loadIssueDetail(currentIssueId);
    setQuery();
  }, [currentIssueId]);

  const handleCopyIssue = (issue) => {
    store.setCopyIssueShow(false);
    loadIssueDetail(issue.issueId);
    if (onIssueCopy) {
      onIssueCopy(issue);
    } else if (onUpdate) {
      onUpdate();
    }
  };

  const handleRelateStory = () => {
    store.setRelateStoryShow(false);
    if (onUpdate) {
      onUpdate();
    }
    loadIssueDetail();
  };

  const handleTransformSubIssue = () => {
    store.setTransformSubIssueShow(false);
    if (onUpdate) {
      onUpdate();
    }
    loadIssueDetail();
  };

  const handleTransformFromSubIssue = () => {
    store.setTransformFromSubIssueShow(false);
    if (onUpdate) {
      onUpdate();
    }
    loadIssueDetail();
  };

  const handleResizeEnd = ({ width }) => {
    localStorage.setItem('agile.EditIssue.width', `${width}px`);
  };

  useImperativeHandle(forwardedRef, () => ({
    loadIssueDetail,
  }));

  const handleResize = throttle(({ width }) => {
    setQuery(width);
    if (onChangeWidth) {
      onChangeWidth(width);// 设置宽度
    }
  }, 150);

  const issue = store.getIssue;
  const {
    issueId, issueNum, summary,
    assigneeId, objectVersionNumber, createdBy, typeCode,
  } = issue;
  const linkIssues = store.getLinkIssues;

  const {
    getChangeParentShow: changeParentShow,
    getAssigneeShow: assigneeShow,
    getCopyIssueShow: copyIssueShow,
    getTransformSubIssueShow: transformSubIssueShow,
    getTransformFromSubIssueShow: transformFromSubIssueShow,
    getRelateStoryShow: relateStoryShow,
  } = store;
  const rightDisabled = disabled || (IsInProgramStore.isInProgram && typeCode === 'issue_epic');
  const HasPermission = (isOwner || createdBy === AppState.userInfo.id);
  return (
    <div style={{
      position: 'fixed',
      right: 0,
      // eslint-disable-next-line no-nested-ternary
      top: isFullScreen ? 0 : HeaderStore.announcementClosed ? 50 : 100,
      bottom: 0,
      // height: 'calc(100vh - 50px)',
      zIndex: 101,
      overflow: 'hidden',
    }}
    >
      <ResizeAble
        modes={['left']}
        size={{
          maxWidth: window.innerWidth * 0.6,
          minWidth: 440,
        }}
        defaultSize={{
          width: localStorage.getItem('agile.EditIssue.width') || 605,
          height: '100%',
        }}
        onResizeEnd={handleResizeEnd}
        onResize={handleResize}
      >
        <div className={`${prefixCls}`} style={style} ref={container}>
          <div className={`${prefixCls}-divider`} />
          {
            issueLoading ? (
              <div
                style={{
                  position: 'absolute',
                  top: 0,
                  bottom: 0,
                  left: 0,
                  right: 0,
                  background: 'rgba(255, 255, 255, 0.65)',
                  zIndex: 9999,
                  display: 'flex',
                  justifyContent: 'center',
                  alignItems: 'center',
                }}
              >
                <Spin />
              </div>
            ) : null
          }
          <div className="c7n-content">
            <IssueHeader
              disabled={rightDisabled}
              store={store}
              reloadIssue={loadIssueDetail}
              backUrl={backUrl}
              onCancel={onCancel}
              loginUserId={AppState.userInfo.id}
              hasPermission={HasPermission}
              onDeleteIssue={onDeleteIssue}
              onUpdate={onUpdate}
            />
            <IssueBody
              key={issueId}
              disabled={rightDisabled}
              store={store}
              reloadIssue={loadIssueDetail}
              onUpdate={onUpdate}
              onDeleteSubIssue={onDeleteSubIssue}
              loginUserId={AppState.userInfo.id}
              hasPermission={isOwner}
              applyType={applyType}
              onDeleteIssue={onDeleteIssue}
              parentSummary={summary}
            />
          </div>
          {
            copyIssueShow ? (
              <CopyIssue
                issueId={issueId}
                issueNum={issueNum}
                issue={issue}
                issueLink={linkIssues}
                issueSummary={summary}
                visible={copyIssueShow}
                onCancel={() => store.setCopyIssueShow(false)}
                onOk={handleCopyIssue.bind(this)}
                applyType={applyType}
              />
            ) : null
          }
          {
            relateStoryShow ? (
              <RelateStory
                issue={issue}
                visible={relateStoryShow}
                onCancel={() => store.setRelateStoryShow(false)}
                onOk={handleRelateStory.bind(this)}
              />
            ) : null
          }
          {
            transformSubIssueShow ? (
              <TransformSubIssue
                visible={transformSubIssueShow}
                issueId={issueId}
                issueNum={issueNum}
                ovn={objectVersionNumber}
                onCancel={() => store.setTransformSubIssueShow(false)}
                onOk={handleTransformSubIssue.bind(this)}
                issueTypes={issueTypes}
              />
            ) : null
          }
          {
            transformFromSubIssueShow ? (
              <TransformFromSubIssue
                visible={transformFromSubIssueShow}
                issueId={issueId}
                issueNum={issueNum}
                ovn={objectVersionNumber}
                onCancel={() => store.setTransformFromSubIssueShow(false)}
                onOk={handleTransformFromSubIssue.bind(this)}
                store={store}
              />
            ) : null
          }
          {
            changeParentShow ? (
              <ChangeParent
                issueId={issueId}
                issueNum={issueNum}
                visible={changeParentShow}
                objectVersionNumber={objectVersionNumber}
                onOk={() => {
                  store.setChangeParentShow(false);
                  if (onUpdate) {
                    onUpdate();
                  }
                  loadIssueDetail(issueId);
                }}
                onCancel={() => {
                  store.setChangeParentShow(false);
                }}
              />
            ) : null
          }
        </div>
      </ResizeAble>
    </div>
  );
}
EditIssue.defaultProps = defaultProps;
export default observer(EditIssue);
