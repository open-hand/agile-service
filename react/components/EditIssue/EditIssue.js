/* eslint-disable react/jsx-no-bind */
/* eslint-disable react/sort-comp */
import React, {
  useContext, useState, useEffect, useImperativeHandle, useRef, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import { stores, Choerodon, WSHandler } from '@choerodon/boot';
import { usePersistFn } from 'ahooks';
import { Spin } from 'choerodon-ui/pro';
import JSONbig from 'json-bigint';
import { reverse } from 'lodash';
import openCreateSubTask from '@/components/create-sub-task';
import './EditIssue.less';
import {
  issueApi, fieldApi, issueLinkApi, workLogApi, knowledgeApi, dataLogApi, pageConfigApi, boardApi,
} from '@/api';
import useIsInProgram from '@/hooks/useIsInProgram';
import { useDetailContainerContext } from '@/components/detail-container/context';
import { sameProject } from '@/utils/detail';
import IssueHeader from './IssueComponent/IssueHeader';
import IssueBody from './IssueComponent/IssueBody/IssueBody';
import EditIssueContext from './stores';
import { getProjectId } from '@/utils/common';

const JSONbigString = JSONbig({ storeAsString: true });

// 项目加入群之后，不关联自己的史诗和特性，只能关联项目群的，不能改关联的史诗
const { AppState } = stores;

const defaultProps = {
  applyType: 'agile',
};

function EditIssue() {
  const [issueLoading, setIssueLoading] = useState(false);
  const {
    outside,
    projectId,
    organizationId,
    afterIssueUpdate,
    store,
    forwardedRef,
    issueId: currentIssueId,
    applyType,
    programId,
    backUrl,
    style,
    disabled,
    prefixCls,
    issueStore,
    transformIssue,
    setSelect,
    descriptionEditRef,
  } = useContext(EditIssueContext);
  const otherProject = !sameProject(projectId);
  const container = useRef();
  const idRef = useRef();
  const { push, close, eventsMap } = useDetailContainerContext();
  const issueEvents = eventsMap.get(applyType === 'program' ? 'program_issue' : 'issue');
  const onUpdate = useCallback((issue) => {
    issueEvents?.update(issue);
  }, [issueEvents]);
  const onCancel = useCallback(() => {
    close();
  }, [close]);
  const onIssueCopy = useCallback((issue) => {
    const callback = issueEvents?.copy || issueEvents?.update;
    if (callback) {
      callback(issue);
    }
  }, [issueEvents]);
  const onCreateSubIssue = useCallback((subIssue, parentIssueId) => {
    if (issueEvents?.createSubIssue) {
      issueEvents?.createSubIssue(subIssue, parentIssueId);
    } else if (issueEvents?.update) {
      issueEvents?.update();
    }
  }, [issueEvents]);
  const onDeleteIssue = useCallback((issue) => {
    const callback = issueEvents?.delete || issueEvents?.update;
    if (callback) {
      callback(issue);
    }
    close();
  }, [close, issueEvents]);
  const onDeleteSubIssue = useCallback((issue, subIssueId) => {
    const callback = issueEvents?.deleteSubIssue;
    if (callback) {
      callback(issue, subIssueId);
    }
    close();
  }, [close, issueEvents]);
  const onTransformType = useCallback((newIssue, oldIssue) => {
    if (issueEvents?.transformType) {
      issueEvents?.transformType(newIssue, oldIssue);
    } else if (issueEvents?.update) {
      issueEvents?.update(newIssue);
    }
  }, [issueEvents]);
  const onChangeParent = useCallback((newIssue) => {
    if (issueEvents?.changeParent) {
      issueEvents?.changeParent(newIssue, store.getIssue);
    } else if (issueEvents?.update) {
      issueEvents?.update(newIssue);
    }
    loadIssueDetail(issueId);
  }, [issueEvents, store.getIssue]);
  const onLinkIssue = useCallback((res) => {
    if (issueEvents?.linkIssue) {
      issueEvents?.linkIssue(res);
    } else if (issueEvents?.update) {
      issueEvents?.update(store.getIssue);
    }
  }, [issueEvents, store.getIssue]);

  const loadIssueDetail = async (paramIssueId, callback) => {
    const id = paramIssueId || idRef.current || currentIssueId;
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
      let issue;
      try {
        issue = await (programId
          ? issueApi.project(projectId).loadUnderProgram(id, programId) : issueApi.org(organizationId).outside(outside).project(projectId).load(id));
      } catch (error) {
        if (error.code === 'error.issue.null') {
          close();
          return;
        }
      }
      // 1. 加载详情

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
        issueTypeId: issue.issueTypeId,
        // context: issue.typeCode,
        pageCode: 'agile_issue_edit',
      };
      const fields = await fieldApi.project(programId ?? projectId).org(organizationId).outside(outside).getFieldAndValue(id, param);

      // 根据问题类型查询rules
      if (!outside && !otherProject && String(issue?.projectId) === String(getProjectId())) {
        const rules = await pageConfigApi.project(programId ?? projectId).org(organizationId).getCascadeRuleList(issue.issueTypeId);
        store.setIssueTypeRules(rules);
      }

      const { description, issueTypeVO: { typeCode, id: typeId } } = issue;
      if (!disabled && (!description || description === JSON.stringify([{ insert: '\n' }]))) { // 加载默认模板
        const issueTemplateInfo = await pageConfigApi.project(projectId).loadTemplateByType(issue.issueTypeId) || {};
        const { template } = issueTemplateInfo;
        issue.descriptionTemplate = template;
      }
      setIssueLoading(false);
      if (callback) {
        callback();
      }
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
        comments,
        notAllowedTransferStatus,
      ] = await Promise.all([
        otherProject || outside ? null : knowledgeApi.project(projectId).loadByIssue(id),
        otherProject || outside || programId || applyType === 'program' ? null : workLogApi.project(projectId).loadByIssue(id),
        programId ? dataLogApi.loadUnderProgram(id, programId) : dataLogApi.org(organizationId).outside(outside).project(projectId).loadByIssue(id),
        programId || applyType === 'program' ? null : issueLinkApi.org(organizationId).outside(outside).project(projectId).loadByIssueAndApplyType(id),
        programId ? issueApi.project(projectId).getCommentsUnderProgram(id, programId) : issueApi.org(organizationId).outside(outside).project(projectId).getComments(id),
        // issue中非子任务的问题需要请求不能流转到的状态数据
        applyType !== 'program' && issue.typeCode !== 'sub_task' ? boardApi.getNotAllowedTransferStatus(id) : null,
      ]);
      if (idRef.current !== id) {
        return;
      }
      store.initIssueAttribute(doc, workLogs, dataLogs, linkIssues, comments);
      store.setNotAllowedTransferStatus(notAllowedTransferStatus);
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

  // 缺陷转子缺陷
  const onRelateIssue = (issue) => {
    onTransformType(issue, store.getIssue);
    loadIssueDetail();
  };

  const handleTransformSubIssue = (newIssue) => {
    onTransformType(newIssue, store.getIssue);
    loadIssueDetail();
  };

  useImperativeHandle(forwardedRef, () => ({
    loadIssueDetail,
  }));

  const issue = store.getIssue;
  const {
    issueId, issueNum, summary,
    assigneeId, objectVersionNumber, createdBy, typeCode, issueTypeId,
  } = issue;

  const { isInProgram } = useIsInProgram();
  const rightDisabled = disabled || (isInProgram && (typeCode === 'issue_epic' || typeCode === 'feature'));
  useEffect(() => {
    function updateBefore() {
      store.setUpdateLoaded(false);
      setIssueLoading(true);
    }
    async function updateAfter(result) {
      const failed = typeof (result) === 'object' && result ? result.failed : false;
      if (failed) {
        setIssueLoading(false);
        throw result;
      }
      if (onUpdate) {
        onUpdate(result);
      }
      if (loadIssueDetail) {
        await loadIssueDetail(issueId);
        store.setUpdateLoaded(true);
      }
      return result;
    }
    store.events = { updateAfter, updateBefore };
  }, [issueId, loadIssueDetail, onUpdate, store]);
  const resetDefault = useCallback(() => {
    store.setDefaultSummary(undefined);
    store.setDefaultTypeId(undefined);
    store.setDefaultSprint(undefined);
    store.setDefaultAssignee(undefined);
  }, [store]);

  const handleOpenCreateSubTask = usePersistFn(() => {
    const {
      issueId: parentIssueId, summary: parentSummary, activeSprint,
    } = store.getIssue;
    openCreateSubTask({
      onCreate: (subIssue) => {
        resetDefault();
        if (onCreateSubIssue) {
          onCreateSubIssue(subIssue, parentIssueId);
        }
        loadIssueDetail(subIssue.issueId);
      },
      parentIssue: {
        summary: parentSummary,
        issueId: parentIssueId,
      },
      defaultValues: {
        summary: store.defaultSummary,
        sprint: activeSprint ? activeSprint.sprintId : undefined,
      },
      defaultAssignee: store.defaultAssignee,
      defaultTypeId: store.defaultTypeId,
    });
  });
  const handleOpenCreateSubBug = usePersistFn(() => {
    const {
      issueId: parentIssueId, summary: parentSummary, activeSprint,
    } = store.getIssue;
    openCreateSubTask({
      typeCode: 'bug',
      onCreate: (subIssue) => {
        resetDefault();
        if (onCreateSubIssue) {
          onCreateSubIssue(subIssue, parentIssueId);
        }
        loadIssueDetail(subIssue.issueId);
      },
      parentIssue: {
        summary: parentSummary,
        issueId: parentIssueId,
      },
      defaultValues: {
        summary: store.defaultSummary,
        sprint: activeSprint ? activeSprint.sprintId : undefined,
      },
      defaultAssignee: store.defaultAssignee,
      defaultTypeId: store.defaultTypeId,
    });
  });

  const handleMessage = usePersistFn((message) => {
    const data = JSONbigString.parse(message);
    if (data.issue?.issueId === issueId) {
      store.setUpdateMessage({ ...store.updateMessage, ...(data.issue || {}) });
      store.setUpdateFieldsAndValue([...store.updateFieldsAndValue, ...(data.customFields || [])]);

      if (store.updateLoaded) {
        const newFields = store.fields.map((item) => {
          const newFieldAndValue = reverse(store.updateFieldsAndValue || []).find((field) => field.fieldId === item.fieldId);
          if (newFieldAndValue) {
            return newFieldAndValue;
          }
          return item;
        });
        store.setIssueFields({ ...issue, ...store.updateMessage }, newFields);
        store.setUpdateMessage({});
        store.setUpdateFieldsAndValue([]);
      }
    }
  });

  return (

    <div className={`${prefixCls}`} style={style} ref={container}>
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
          otherProject={otherProject || String(issue?.projectId) !== String(getProjectId())}
          outside={outside}
          onTransformType={onTransformType}
          onOpenCreateSubTask={handleOpenCreateSubTask}
          onOpenCreateSubBug={handleOpenCreateSubBug}
        />
        <IssueBody
          setIssueLoading={setIssueLoading}
          outside={outside}
          key={issueId}
          projectId={projectId}
          organizationId={organizationId}
          disabled={rightDisabled}
          store={store}
          issueId={idRef.current}
          programId={programId}
          reloadIssue={loadIssueDetail}
          onUpdate={onUpdate}
          onIssueCopy={onIssueCopy}
          onCreateSubIssue={onCreateSubIssue}
          onDeleteSubIssue={onDeleteSubIssue}
          onLinkIssue={onLinkIssue}
          loginUserId={AppState.userInfo.id}
          applyType={applyType}
          onDeleteIssue={onDeleteIssue}
          parentSummary={summary}
          push={push}
          otherProject={otherProject || String(issue?.projectId) !== String(getProjectId())}
          onChangeParent={onChangeParent}
          onRelateIssue={onRelateIssue}
          onTransformSubIssue={handleTransformSubIssue}
          onOpenCreateSubTask={handleOpenCreateSubTask}
          onOpenCreateSubBug={handleOpenCreateSubBug}
        />
        <WSHandler
          messageKey={`agile-issue-update-by-trigger-${getProjectId()}`}
          onMessage={handleMessage}
        >
          <div />
        </WSHandler>
      </div>
    </div>
  );
}
EditIssue.defaultProps = defaultProps;
export default observer(EditIssue);
