import React, {
  useMemo, useState, useEffect, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, Modal, Button,
} from 'choerodon-ui/pro';
import { Steps } from 'choerodon-ui';
import {
  includes, map, compact, uniq,
} from 'lodash';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { Choerodon } from '@choerodon/boot';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import { usePersistFn } from 'ahooks';
import {
  issueTypeApi, projectApi, moveIssueApi, commonApi,
} from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import {
  IModalProps, IIssueType, Issue,
} from '@/common/types';
import SelectProject from './components/select-project';
import Confirm from './components/confirm-data';
import styles from './IssueMove.less';
import { IssueWithSubIssueVOList, ILoseItems } from './components/confirm-data/Confirm';
import { submitFieldMap } from './transformValue';

import store, { FieldWithValue } from './store';

const isDEV = process.env.NODE_ENV === 'development';
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
const shouldRequest = isDEV || HAS_AGILE_PRO;
const { Step } = Steps;

interface Props {
  issue: IssueWithSubIssueVOList,
  modal?: IModalProps
  projectId?:string
  fieldsWithValue: FieldWithValue[]
  onMoveIssue: (issue: Issue) => void,
  loseItems: ILoseItems,
}

const IssueMove: React.FC<Props> = ({
  modal, issue, fieldsWithValue, onMoveIssue, loseItems, projectId,
}) => {
  const [, setUpdateCount] = useState<number>(0);
  const [currentStep, setCurrentStep] = useState<number>(1);
  const [btnLoading, setBtnLoading] = useState<boolean>(false);
  const [targetProjectType, setTargetProjectType] = useState<'program' | 'project' | 'subProject'>('project');
  const issueTypeDataSet = useMemo(() => new DataSet({
    paging: false,
    data: [],
  }), []);

  const removeField = useCallback((ds: DataSet, name: string) => {
    ds?.fields?.delete(name);
    ds?.current?.fields.delete(name);
  }, []);

  const resetData = useCallback((ds: DataSet, excludeFieldsCode: string[]) => {
    const fieldNames: string[] = [];
    (ds.current?.fields || []).forEach((field: Field) => {
      if (!includes(excludeFieldsCode, field.get('name'))) {
        field.reset();
        ds.current?.set(field.get('name'), undefined);
        fieldNames.push(field.get('name'));
      }
    });
    fieldNames.forEach((name: string) => {
      removeField(ds, name);
    });
  }, [removeField]);

  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'targetProjectId',
        textField: 'name',
        valueField: 'id',
        label: '选择目标项目',
        required: true,
      },
      {
        name: 'issueType',
        textField: 'name',
        valueField: 'id',
        label: '选择目标类型',
        options: issueTypeDataSet,
        required: true,
        dynamicProps: {
          disabled: ({ record }) => issue.typeCode === 'feature' || !record.get('targetProjectId'),
        },
      }, {
        name: 'subTaskIssueTypeId',
        textField: 'name',
        valueField: 'id',
        label: '选择子任务类型',
        options: issueTypeDataSet,
        dynamicProps: {
          disabled: ({ record }) => !record.get('targetProjectId'),
          required: () => issue.subIssueVOList?.length > 0,
        },
      }, {
        name: 'subBugIssueTypeId',
        textField: 'name',
        valueField: 'id',
        label: '选择子缺陷类型',
        options: issueTypeDataSet,
        dynamicProps: {
          disabled: ({ record }) => !record.get('targetProjectId'),
          required: () => issue.subBugVOList?.length > 0,
        },
      },
    ],
    events: {
      update: async ({
        // @ts-ignore
        dataSet: moveDataSet, name, value,
      }) => {
        if (name === 'targetProjectId') {
          if (value) {
            const targetProjectInfo = await projectApi.loadBasicInfo(value);
            let targetIsInProgram = false;
            const targetIsProgram = (targetProjectInfo.categories || []).find((item: any) => item.code === 'N_PROGRAM');
            if (!targetIsInProgram && shouldRequest) {
              targetIsInProgram = Boolean(await commonApi.getProjectsInProgram(value));
            }
            const issueTypes: IIssueType[] = await issueTypeApi.loadAllWithStateMachineId(targetIsProgram ? 'program' : 'agile', value);
            const excludeTypeCode: string[] = [];
            if (!targetIsProgram) {
              excludeTypeCode.push('feature');
              if (targetIsInProgram) {
                setTargetProjectType('subProject');
                excludeTypeCode.push('issue_epic');
              } else {
                setTargetProjectType('project');
              }
              // if (issue.subIssueVOList && issue.subIssueVOList.length) {
              //   excludeTypeCode.push('bug');
              // }
            } else {
              setTargetProjectType('program');
            }
            await issueTypeDataSet.loadData(issueTypes.filter((item) => !includes(excludeTypeCode, item.typeCode)));
          } else {
            issueTypeDataSet.loadData([]);
          }
          if (issue.typeCode === 'feature') {
            moveDataSet.current.set('issueType', 'feature');
          } else if (moveDataSet.current?.get('issueType')) {
            moveDataSet.current.set('issueType', undefined);
          }
        }
        if (name === 'targetProjectId' || name === 'issueType' || name === 'subTaskIssueTypeId' || name === 'subBugIssueTypeId') {
          resetData(moveDataSet, ['targetProjectId', 'issueType', 'subTaskIssueTypeId', 'subBugIssueTypeId']); // 改变项目或者工作项类型应该重置
        }
        if (name === 'subTaskIssueTypeId') {
          store.setSubTaskTypeId(value);
        }
        if (name === 'subBugIssueTypeId') {
          store.setSubBugTypeId(value);
        }
        setUpdateCount((count) => count + 1);
      },
    },
  }), [issue.subBugVOList, issue.subIssueVOList, issue.typeCode, issueTypeDataSet, resetData]);

  const handlePre = () => {
    setCurrentStep(currentStep - 1);
  };
  const handleNext = async () => {
    Promise.all([dataSet.current?.getField('targetProjectId')?.checkValidity(), dataSet.current?.getField('issueType')?.checkValidity(), dataSet.current?.getField('subTaskIssueTypeId')?.checkValidity(), dataSet.current?.getField('subBugIssueTypeId')?.checkValidity()]).then((validateRes) => {
      if (validateRes.every((validate) => !!validate)) {
        setCurrentStep(currentStep + 1);
      }
    });
  };
  const handleCancel = () => {
    modal?.close();
  };

  useEffect(() => {
    const userIds = [issue.assigneeId, issue.reporterId, issue.mainResponsible?.id, ...(map(fieldsWithValue.filter((item) => !item.system && item.fieldType === 'member' && !item.projectId), 'value'))];
    const uniqUserIds = uniq(compact(userIds));
    store.setSelectUserIds(uniqUserIds);
  }, [fieldsWithValue, issue.assigneeId, issue.mainResponsible?.id, issue.reporterId]);

  const { valueReady } = store;
  const targetTypeId = dataSet.current?.get('issueType');
  const subTaskIssueTypeId = dataSet.current?.get('subTaskIssueTypeId');
  const subBugIssueTypeId = dataSet.current?.get('subBugIssueTypeId');
  const targetProjectId = dataSet.current?.get('targetProjectId');

  const handleSubmit = usePersistFn(() => {
    setBtnLoading(true);

    const result = new Map();

    store.issueMap.forEach((item, issueId) => {
      const { target } = item;
      const targetIssue = target.issue;
      const getValue = (code: string) => targetIssue.customFields.get(code)?.value;
      const targetValue: any = {
        issueId,
        issueTypeId: targetIssue.issueTypeVO.id,
      };
      [...submitFieldMap.entries()].forEach(([key, idKey]) => {
        targetValue[idKey] = getValue(key);
        if (key === 'fixVersion') {
          targetValue.versionType = 'fix';
        }
      });
      targetValue.customFields = [...targetIssue.customFields.values()].filter((f) => !f.system);
      result.set(issueId, targetValue);
    });
    const mainIssue = result.get(issue.issueId);
    const submitData: any = {
      ...mainIssue,
      subIssues: [...issue.subIssueVOList, ...issue.subBugVOList].map((i) => result.get(i.issueId)),
    };
    moveIssueApi.project(projectId).moveIssueToProject(issue.issueId, targetProjectId, submitData).then(() => {
      onMoveIssue(issue as any);
      Choerodon.prompt('移动成功');
      setBtnLoading(false);
      modal?.close();
    }).catch(() => {
      setBtnLoading(false);
      Choerodon.prompt('移动失败');
    });
    return false;
  });

  const targetIssueType = useMemo(() => issueTypeDataSet.toData().find((item: IIssueType) => item.id === targetTypeId) as IIssueType, [issueTypeDataSet, targetTypeId]);
  const targetSubTaskType = useMemo(() => issueTypeDataSet.toData().find((item: IIssueType) => item.id === subTaskIssueTypeId) as IIssueType, [issueTypeDataSet, subTaskIssueTypeId]);
  const targetSubBugType = useMemo(() => issueTypeDataSet.toData().find((item: IIssueType) => item.id === subBugIssueTypeId) as IIssueType, [issueTypeDataSet, subBugIssueTypeId]);

  return (
    <div className={styles.issueMove}>
      <div style={{ padding: '20px 20px 0' }}>
        <Steps current={currentStep - 1}>
          <Step
            title={<span style={{ color: currentStep === 1 ? '#5365EA' : '', fontSize: 14 }}>选择项目和工作项类型</span>}
          />
          <Step
            title={<span style={{ color: currentStep === 2 ? '#5365EA' : '', fontSize: 14 }}>确认数据信息</span>}
          />
        </Steps>
      </div>
      <div className={styles.step_content}>
        {currentStep === 1 && <SelectProject issue={issue} dataSet={dataSet} issueTypeDataSet={issueTypeDataSet} projectId={projectId} />}
        {currentStep === 2 && (
          <Confirm
            issue={issue}
            dataSet={dataSet}
            fieldsWithValue={fieldsWithValue}
            targetProjectType={targetProjectType}
            targetIssueType={targetIssueType}
            targetSubTaskType={targetSubTaskType}
            targetSubBugType={targetSubBugType}
            loseItems={loseItems}
          />
        )}
      </div>
      <div className="c7n-pro-modal-footer c7n-pro-modal-footer-drawer">
        {currentStep === 1 && (
          <>
            <Button onClick={handleCancel} funcType={'raised' as FuncType}>
              取消
            </Button>
            <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleNext}>
              下一步
            </Button>
          </>
        )}
        {currentStep === 2 && (
          <>
            <Button style={{ marginLeft: 8 }} color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handlePre}>
              上一步
            </Button>
            <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleSubmit} disabled={!valueReady} loading={btnLoading}>
              确认
            </Button>
            <Button onClick={handleCancel} funcType={'raised' as FuncType}>
              取消
            </Button>
          </>
        )}
      </div>
    </div>
  );
};

const ObserverIssueMove = observer(IssueMove);

const openIssueMove = ({
  issue, customFields, onMoveIssue, loseItems,
}: { issue: IssueWithSubIssueVOList, customFields: FieldWithValue[], onMoveIssue: (issue: Issue) => void, loseItems: ILoseItems }) => {
  Modal.open({
    key: 'issueMoveModal',
    drawer: true,
    title: '移动工作项',
    className: styles.issueMoveModal,
    style: {
      width: MODAL_WIDTH.middle,
    },
    children: <ObserverIssueMove issue={issue} fieldsWithValue={customFields} onMoveIssue={onMoveIssue} loseItems={loseItems} />,
    footer: null,
  });
};

export default openIssueMove;
