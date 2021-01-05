import React, {
  useMemo, useImperativeHandle, useState, useEffect, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, Radio, DataSet, Modal, Button,
} from 'choerodon-ui/pro';
import { Steps } from 'choerodon-ui';
import { includes } from 'lodash';
import {
  IModalProps, Issue, AppStateProps, IIssueType,
} from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { toJS } from 'mobx';
import { stores } from '@choerodon/boot';
import useIsInProgram from '@/hooks/useIsInProgram';
import {
  issueTypeApi, projectApi, moveIssueApi, commonApi,
} from '@/api';
import SelectProject from './components/select-project';
import Confirm from './components/confirm-data';
import Finish from './components/finish';
import styles from './IssueMove.less';
import { IssueWithSubIssueVOList } from './components/confirm-data/Confirm';
import { IFieldWithValue } from './components/confirm-data/transformValue';

const isDEV = process.env.NODE_ENV === 'development';
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
const shouldRequest = isDEV || HAS_AGILE_PRO;
const { AppState }: { AppState: AppStateProps } = stores;
const { Step } = Steps;

interface Props {
  issue: IssueWithSubIssueVOList,
 modal?:IModalProps
 fieldsWithValue: IFieldWithValue[]
}

const IssueMove: React.FC<Props> = ({
  modal, issue, fieldsWithValue,
}) => {
  const { isInProgram, loading } = useIsInProgram();
  const [updateCount, setUpdateCount] = useState<number>(0);
  const [currentStep, setCurrentStep] = useState<number>(1);
  const [step1NextDisabled, setsStep1NextDisabled] = useState<boolean>(true);
  const [targetProjectType, setTargetProjectType] = useState<'program' | 'project' | 'subProject'>('project');

  const issueTypeDataSet = useMemo(() => new DataSet({
    paging: false,
    data: [],
  }), []);

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
        valueField: 'typeCode',
        label: '选择目标类型',
        options: issueTypeDataSet,
        required: true,
        dynamicProps: {
          disabled: ({ record }) => issue.typeCode === 'feature' || !record.get('targetProjectId'),
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
            const targetIsProgram = targetProjectInfo.category === 'PROGRAM';
            if (!targetIsInProgram && shouldRequest) {
              targetIsInProgram = Boolean(await commonApi.getProjectsInProgram(value));
            }
            const issueTypes: IIssueType[] = await issueTypeApi.loadAllWithStateMachineId(targetIsProgram ? 'program' : 'agile', value);
            const excludeTypeCode: string[] = ['sub_task'];
            if (!targetIsProgram) {
              excludeTypeCode.push('feature');
              if (targetIsInProgram) {
                setTargetProjectType('subProject');
                excludeTypeCode.push('issue_epic');
              } else {
                setTargetProjectType('project');
              }
              if (issue.subIssueVOList && issue.subIssueVOList.length) {
                excludeTypeCode.push('bug');
              }
            } else {
              setTargetProjectType('program');
            }
            await issueTypeDataSet.loadData(issueTypes.filter((item) => !includes(excludeTypeCode, item.typeCode)));
            if (issue.typeCode === 'feature') {
              moveDataSet.current.set('issueType', 'feature');
            }
          } else {
            issueTypeDataSet.loadData([]);
          }
          moveDataSet.current.set('issueType', undefined);
        }
        if (name === 'targetProjectId' || name === 'issueType') {
          Promise.all([moveDataSet.current?.getField('targetProjectId')?.checkValidity(), moveDataSet.current?.getField('issueType')?.checkValidity()]).then((validateRes) => {
            setsStep1NextDisabled(!validateRes.every((validate) => !!validate));
          });
        }
        setUpdateCount((count) => count + 1);
      },
    },
  }), [issue.subIssueVOList, issue.typeCode, issueTypeDataSet]);

  const handlePre = () => {
    setCurrentStep(currentStep - 1);
  };
  const handleNext = async () => {
    setCurrentStep(currentStep + 1);
  };
  const handleCancel = () => {
    dataSet.reset();
    modal?.close();
  };

  const handleSubmit = useCallback(async () => false, []);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  const targetTypeCode = dataSet.current?.get('issueType');
  const targetIssueType = {
    typeCode: targetTypeCode,
    // @ts-ignore
    issueTypeId: targetTypeCode && issueTypeDataSet.toData().find((item: IIssueType) => item.typeCode === targetTypeCode)?.id,
  };

  return (
    <div className={styles.issueMove}>
      <Steps current={currentStep - 1}>
        <Step
          title={<span style={{ color: currentStep === 1 ? '#3F51B5' : '', fontSize: 14 }}>选择项目和问题类型</span>}
        />
        <Step
          title={<span style={{ color: currentStep === 2 ? '#3F51B5' : '', fontSize: 14 }}>确认数据信息</span>}
        />
        <Step
          title={<span style={{ color: currentStep === 3 ? '#3F51B5' : '', fontSize: 14 }}>完成</span>}
        />
      </Steps>
      <div className={styles.step_content}>
        {currentStep === 1 && <SelectProject dataSet={dataSet} issueTypeDataSet={issueTypeDataSet} />}
        {currentStep === 2 && <Confirm issue={issue} dataSet={dataSet} fieldsWithValue={fieldsWithValue} targetProjectType={targetProjectType} targetIssueType={targetIssueType} />}
        {currentStep === 3 && <Finish />}
      </div>
      <div className={styles.steps_action}>
        {currentStep === 1 && (
          <>
            <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleNext} disabled={step1NextDisabled}>
              下一步
            </Button>
            <Button onClick={handleCancel} funcType={'raised' as FuncType}>
              取消
            </Button>
          </>
        )}
        {currentStep === 2 && (
          <>
            <Button style={{ marginLeft: 8 }} color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handlePre}>
              上一步
            </Button>
            <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleNext}>
              下一步
            </Button>
            <Button onClick={handleCancel} funcType={'raised' as FuncType}>
              取消
            </Button>
          </>
        )}
        {currentStep === 3 && (
          <>
            <Button style={{ marginLeft: 8 }} color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handlePre}>
              上一步
            </Button>
            <Button style={{ marginLeft: 8 }} color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleSubmit}>
              确定
            </Button>
            <Button onClick={handleCancel}>
              取消
            </Button>
          </>
        )}
      </div>
    </div>
  );
};

const ObserverIssueMove = observer(IssueMove);

const openIssueMove = ({ issue, customFields }: { issue: IssueWithSubIssueVOList, customFields: IFieldWithValue[] }) => {
  Modal.open({
    key: 'issueMoveModal',
    drawer: true,
    title: '移动问题',
    className: styles.issueMoveModal,
    style: {
      width: MODAL_WIDTH.middle,
    },
    children: <ObserverIssueMove issue={issue} fieldsWithValue={customFields} />,
    footer: null,
  });
};

export default openIssueMove;
