import React, {
  useMemo, useEffect, useImperativeHandle, useState, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  SelectBox, DataSet, Button, TextField, Icon,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { FuncType } from 'choerodon-ui/pro/lib/button/interface';
import { includes } from 'lodash';
import { fieldApi } from '@/api';
import { getApplyType } from '@/utils/common';
import useIsInProgram from '@/hooks/useIsInProgram';
import styles from './ImportFields.less';

const programImportRequiresFields = ['issueType', 'summary', 'description', 'reporter', 'epic', 'epicName', 'pi'];
const projectImportRequiresFields = ['issueType', 'parentIssue', 'epic', 'component', 'sprint', 'summary', 'description', 'epicName', 'assignee', 'reporter', 'priority', 'remainingTime', 'storyPoints', 'linkIssue'];
const subProjectImportRequiredFields = ['issueType', 'parentIssue', 'feature', 'component', 'sprint', 'summary', 'description', 'assignee', 'reporter', 'priority', 'remainingTime', 'storyPoints', 'linkIssue'];

const programSystemFields = [
  { code: 'issueType', title: '工作项类型' },
  { code: 'summary', title: '概要' },
  { code: 'description', title: '描述' },
  { code: 'reporter', title: '报告人' },
  { code: 'epic', title: '所属史诗' },
  { code: 'epicName', title: '史诗名称' },
  { code: 'pi', title: 'PI' },
  { code: 'issueStatus', title: '状态' },
  { code: 'subProject', title: '负责的子项目' },
  { code: 'estimatedStartTime', title: '预计开始时间' },
  { code: 'estimatedEndTime', title: '预计结束时间' },
  { code: 'actualStartTime', title: '实际开始时间' },
  { code: 'actualEndTime', title: '实际结束时间' },
  { code: 'benfitHypothesis', title: '特性价值' },
  { code: 'acceptanceCritera', title: '验收标准' },
  { code: 'programVersion', title: '版本' },
  { code: 'product', title: '产品' },
];

const projectSystemFields = [
  { code: 'issueType', title: '工作项类型' },
  { code: 'summary', title: '概要' },
  { code: 'description', title: '描述' },
  { code: 'parentIssue', title: '父级故事/任务/缺陷' },
  { code: 'assignee', title: '经办人' },
  { code: 'reporter', title: '报告人' },
  { code: 'priority', title: '优先级' },
  { code: 'epic', title: '故事所属史诗' },
  { code: 'component', title: '模块' },
  { code: 'sprint', title: '冲刺' },
  { code: 'epicName', title: '史诗名称' },
  { code: 'remainingTime', title: '剩余预估时间' },
  { code: 'storyPoints', title: '故事点' },
  { code: 'linkIssue', title: '关联工作项' },
  { code: 'issueStatus', title: '状态' },
  { code: 'fixVersion', title: '修复的版本' },
  { code: 'influenceVersion', title: '影响的版本' },
  { code: 'label', title: '标签' },
  { code: 'estimatedStartTime', title: '预计开始时间' },
  { code: 'estimatedEndTime', title: '预计结束时间' },
  { code: 'actualStartTime', title: '实际开始时间' },
  { code: 'actualEndTime', title: '实际结束时间' },
  { code: 'mainResponsible', title: '主要负责人' },
  { code: 'environment', title: '环境' },
  { code: 'participant', title: '参与人' },
  { code: 'estimateTime', title: '原始预估时间' },
  { code: 'product', title: '产品' },
];

const subProjectSystemFields = [
  { code: 'issueType', title: '工作项类型' },
  { code: 'summary', title: '概要' },
  { code: 'description', title: '描述' },
  { code: 'parentIssue', title: '父级故事/任务/缺陷' },
  { code: 'feature', title: '故事所属特性' },
  { code: 'component', title: '模块' },
  { code: 'sprint', title: '冲刺' },
  { code: 'assignee', title: '经办人' },
  { code: 'reporter', title: '报告人' },
  { code: 'priority', title: '优先级' },
  { code: 'remainingTime', title: '剩余预估时间' },
  { code: 'storyPoints', title: '故事点' },
  { code: 'linkIssue', title: '关联工作项' },
  { code: 'issueStatus', title: '状态' },
  { code: 'fixVersion', title: '修复的版本' },
  { code: 'influenceVersion', title: '影响的版本' },
  { code: 'label', title: '标签' },
  { code: 'estimatedStartTime', title: '预计开始时间' },
  { code: 'estimatedEndTime', title: '预计结束时间' },
  { code: 'actualStartTime', title: '实际开始时间' },
  { code: 'actualEndTime', title: '实际结束时间' },
  { code: 'mainResponsible', title: '主要负责人' },
  { code: 'environment', title: '环境' },
  { code: 'participant', title: '参与人' },
  { code: 'estimateTime', title: '原始预估时间' },
  { code: 'product', title: '产品' },
];

interface Props {
  importFieldsRef: React.MutableRefObject<{
    fields: string[]
    allFields: { title: string, code: string, system: boolean }[],
    requiredFields: string[]
    chooseDataSet: DataSet
  }>,
  setReRender: Function,
  applyType?: 'program' | 'agile',
  checkBoxChangeOk: (data: string[]) => void
  requires?: string[]
  systems?: { code: string, title: string }[]
  fields?: { code: string, title: string, system: boolean }[]
}

const ImportFields: React.FC<Props> = ({
  importFieldsRef, setReRender, checkBoxChangeOk, applyType: propsApplyType, requires, systems, fields: fs,
}) => {
  const { isInProgram, loading } = useIsInProgram();
  const [updateCount, setUpdateCount] = useState<number>(0);
  const [requiredFields, setRequiredFields] = useState<string[]>(requires || []);
  const [btnStatus, setBtnStatus] = useState<'ALL' | 'NONE'>();
  const [systemFields, setSystemFields] = useState<{ code: string, title: string }[]>(systems || []);
  const [allFields, setAllFields] = useState<{ code: string, title: string, system: boolean }[]>([]);
  const applyType = propsApplyType ?? getApplyType();
  useEffect(() => {
    if (!systems && !requires) {
      if (!loading) {
        if (applyType === 'program') {
          setRequiredFields(programImportRequiresFields);
          setSystemFields(programSystemFields);
        } else if (isInProgram) {
          setRequiredFields(subProjectImportRequiredFields);
          setSystemFields(subProjectSystemFields);
        } else {
          setRequiredFields(projectImportRequiresFields);
          setSystemFields(projectSystemFields);
        }
      }
    }
  }, [applyType, isInProgram, loading, requires, systems]);

  const fieldsOptionDataSet = useMemo(() => new DataSet({
    paging: false,
    events: {
      load: () => {
        setUpdateCount((count) => count + 1);
        setReRender();
      },
    },
  }), [setReRender]);

  const chooseDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    autoQuery: true,
    fields: [{
      name: 'fields',
      type: 'string' as FieldType,
      textField: 'title',
      valueField: 'code',
      multiple: true,
      options: fieldsOptionDataSet,
    }],
    data: [{
      fields: requiredFields,
    }],
    events: {
      update: ({ value }: { value: string[] }) => {
        checkBoxChangeOk(value);
=        setUpdateCount((count) => count + 1);
        setReRender();
      },
    },
  }), [checkBoxChangeOk, fieldsOptionDataSet, requiredFields, setReRender]);

  useEffect(() => {
    const loadData = async () => {
      const fields = fs || await fieldApi.getFoundationHeader(applyType === 'program' ? 'programIssueType' : 'agileIssueType');
      const allFs = [...(systemFields.map((item) => ({ ...item, system: true }))), ...fields];
      setAllFields(allFs);
      fieldsOptionDataSet.loadData(allFs);
    };

    if ((systemFields && systemFields.length) || fs?.length) {
      loadData();
    }
  }, [fieldsOptionDataSet, fs, systemFields]);

  const fieldsChecked = (chooseDataSet?.current?.get('fields') || requiredFields).filter((code: string) => !includes(['linkIssue', 'parentIssue'], code));
  useImperativeHandle(importFieldsRef, () => ({
    // @ts-ignore
    fields: fieldsOptionDataSet.toData().filter((item) => includes(fieldsChecked, item.code)).map((item) => item.code),
    // @ts-ignore
    allFields: fieldsOptionDataSet.toData(),
    requiredFields,
    chooseDataSet,
  }));
  function handleClick() {
    const result = true;
    const nextBtnStatus = btnStatus !== 'NONE' ? 'NONE' : 'ALL';
    if (nextBtnStatus !== 'ALL') {
      chooseDataSet.current?.set('fields', fieldsOptionDataSet.toData().map((item: any) => item.code));
    } else {
      chooseDataSet.current?.set('fields', requiredFields);
      chooseDataSet.unSelectAll();
    }
    result && setBtnStatus(nextBtnStatus);
  }
  const handleSearch = useCallback((value) => {
    // @ts-ignore
    fieldsOptionDataSet.loadData(allFields.filter((item) => item.title.indexOf(value || '') > -1));
  }, [allFields, fieldsOptionDataSet]);

  return (
    <div className={styles.importFields}>
      <div className={styles.importFields_title}>
        <span>选择字段</span>
        <Button funcType={'flat' as FuncType} className={styles.importFields_btn} onClick={handleClick}>{btnStatus !== 'NONE' ? '全选' : '全不选'}</Button>
      </div>
      <div className={styles.importFields_content}>
        <TextField
          prefix={<Icon type="search" />}
          placeholder="请输入搜索内容"
          style={{ height: 34, width: '100%', marginBottom: 8 }}
          onChange={handleSearch}
          clearButton
        />
        <SelectBox
          dataSet={chooseDataSet}
          name="fields"
          onOption={({ record }) => ({
            disabled: includes(requiredFields, record.get('code')),
          })}
        />
      </div>
    </div>
  );
};

export default observer(ImportFields);
