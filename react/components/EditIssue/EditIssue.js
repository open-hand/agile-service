/* eslint-disable react/jsx-no-bind */
/* eslint-disable react/sort-comp */
import React, {
  useContext, useState, useEffect, useImperativeHandle, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { stores, Choerodon } from '@choerodon/boot';
import { Spin, Modal } from 'choerodon-ui';
import { throttle } from 'lodash';
import './EditIssue.less';
import { useIssueTypes } from '@/hooks';
import {
  issueApi, fieldApi, issueLinkApi, workLogApi, knowledgeApi, dataLogApi, devOpsApi, pageConfigApi,
} from '@/api';
import useIsInProgram from '@/hooks/useIsInProgram';
import RelateStory from '../RelateStory';
import CopyIssue from '../CopyIssue';
import ResizeAble from '../ResizeAble';
import TransformSubIssue from '../TransformSubIssue';
import TransformFromSubIssue from '../TransformFromSubIssue';
import ChangeParent from '../ChangeParent';
import IssueHeader from './IssueComponent/IssueHeader';
import IssueBody from './IssueComponent/IssueBody/IssueBody';
import EditIssueContext from './stores';
// 项目加入群之后，不关联自己的史诗和特性，只能关联项目群的，不能改关联的史诗
const { AppState } = stores;

const defaultProps = {
  applyType: 'agile',
};

function EditIssue() {
  const [issueLoading, setIssueLoading] = useState(false);
  const {
    projectId,
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
    issueStore,
    HeaderStore,
    isFullScreen,
    onChangeWidth,
    transformIssue,
    setSelect,
    descriptionEditRef,
  } = useContext(EditIssueContext);
  const [issueTypes] = useIssueTypes();
  const container = useRef();
  const idRef = useRef();

  const loadIssueDetail = async (paramIssueId) => {
    const id = paramIssueId || currentIssueId;
    if (idRef.current !== id && descriptionEditRef.current) {
      Choerodon.prompt('有未保存的描述');
      return;
    }

    idRef.current = id;
    setIssueLoading(true);
    if (paramIssueId !== currentIssueId) {
      store.setIssue({});
    }
    try {
      // 1. 加载详情
      let issue = await (programId
        ? issueApi.project(projectId).loadUnderProgram(id, programId) : issueApi.project(projectId).load(id));
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
      const fields = await fieldApi.project(projectId).getFieldAndValue(id, param);
      const { description, issueTypeVO: { typeCode } } = issue;
      if (!description || description === JSON.stringify([{ insert: '\n' }])) { // 加载默认模版
        const issueTemplateInfo = await pageConfigApi.project(projectId).loadTemplateByType(typeCode) || {};
        const { template } = issueTemplateInfo;
        issue.descriptionTemplate = template;
      }
      setIssueLoading(false);
      if (transformIssue) {
        issue = transformIssue(issue);
      }
      store.setIssueFields(issue, fields);
      if (issueStore) {
        issueStore.setSelectedIssue(issue);
      }
      if (setSelect) {
        setSelect(issue);
      }
      // 3. 加载额外信息
      const [
        doc,
        workLogs,
        dataLogs,
        linkIssues,
        branches,
      ] = await Promise.all([
        knowledgeApi.project(projectId).loadByIssue(id),
        programId || applyType === 'program' ? null : workLogApi.project(projectId).loadByIssue(id),
        programId ? dataLogApi.loadUnderProgram(id, programId) : dataLogApi.project(projectId).loadByIssue(id),
        programId || applyType === 'program' ? null : issueLinkApi.project(projectId).loadByIssueAndApplyType(id),
        programId || applyType === 'program' ? null : devOpsApi.project(projectId).countBranches(id),
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
  const { isInProgram } = useIsInProgram();
  const rightDisabled = disabled || (isInProgram && (typeCode === 'issue_epic' || typeCode === 'feature'));
  return (
    <div style={{
      position: 'fixed',
      right: 0,
      // eslint-disable-next-line no-nested-ternary
      top: isFullScreen ? 0 : HeaderStore.announcementClosed ? 48 : 100,
      bottom: 0,
      // height: 'calc(100vh - 50px)',
      zIndex: 101,
      // overflow: 'hidden',
    }}
    >
      <ResizeAble
        modes={['left']}
        size={{
          maxWidth: window.innerWidth * 0.6,
          minWidth: 440,
        }}
        defaultSize={{
          width: localStorage.getItem('agile.EditIssue.width') || 640,
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
              onDeleteIssue={onDeleteIssue}
              onUpdate={onUpdate}
            />
            <IssueBody
              key={issueId}
              projectId={projectId}
              disabled={rightDisabled}
              store={store}
              issueId={currentIssueId}
              programId={programId}
              reloadIssue={loadIssueDetail}
              onUpdate={onUpdate}
              onDeleteSubIssue={onDeleteSubIssue}
              loginUserId={AppState.userInfo.id}
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
