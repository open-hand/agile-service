import React, {
  useMemo, useEffect, useImperativeHandle, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import { SelectBox, DataSet } from 'choerodon-ui/pro';
import { fieldApi } from '@/api';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { includes } from 'lodash';
import { getApplyType } from '@/utils/common';
import useIsInProgram from '@/hooks/useIsInProgram';
import styles from './ImportFields.less';

const programImportRequiresFields = ['summary', 'description', 'issueTypeId', 'reporterId', 'epicId', 'epicName', 'piNameVOList'];
const projectImportRequiresFields = ['summary', 'parentIssue', 'epic', 'component', 'sprint', 'summary', 'description', 'epicName', 'assignee', 'reporter', 'priority', 'remainingTime', 'storyPoints', 'linkIssue'];
const subProjectImportRequiredFields = ['summary', 'parentIssue', 'feature', 'component', 'sprint', 'summary', 'description', 'assignee', 'reporter', 'priority', 'remainingTime', 'storyPoints', 'linkIssue'];

const programSystemFields = [
  { code: 'summary', title: '概要' },
  { code: 'description', title: '描述' },
  { code: 'issueTypeId', title: '类型' },
  { code: 'reporterId', title: '报告人' },
  { code: 'epicId', title: '所属史诗' },
  { code: 'epicName', title: '史诗名称' },
  { code: 'piNameVOList', title: 'PI' },
  { code: 'estimatedStartTime', title: '预计开始时间' },
  { code: 'estimatedEndTime', title: '预计结束时间' },
  { code: 'benfitHypothesis', title: '特性价值' },
  { code: 'acceptanceCritera', title: '验收标准' },
  { code: 'teamProjects', title: '负责的子项目' },
];

const projectSystemFields = [
  { code: 'summary', title: '类型' },
  { code: 'parentIssue', title: '父级故事/任务/缺陷' },
  { code: 'epic', title: '故事所属史诗' },
  { code: 'component', title: '模块' },
  { code: 'sprint', title: '冲刺' },
  { code: 'summary', title: '概要' },
  { code: 'description', title: '描述' },
  { code: 'epicName', title: '史诗名称' },
  { code: 'assignee', title: '经办人' },
  { code: 'reporter', title: '报告人' },
  { code: 'priority', title: '优先级' },
  { code: 'remainingTime', title: '预估时间' },
  { code: 'storyPoints', title: '故事点' },
  { code: 'linkIssue', title: '关联问题' },
  { code: 'version', title: '版本' },
  { code: 'label', title: '标签' },
  { code: 'estimatedStartTime', title: '预计开始时间' },
  { code: 'estimatedEndTime', title: '预计结束时间' },
];

const subProjectSystemFields = [
  { code: 'summary', title: '类型' },
  { code: 'parentIssue', title: '父级故事/任务/缺陷' },
  { code: 'feature', title: '故事所属特性' },
  { code: 'component', title: '模块' },
  { code: 'sprint', title: '冲刺' },
  { code: 'summary', title: '概要' },
  { code: 'description', title: '描述' },
  { code: 'assignee', title: '经办人' },
  { code: 'reporter', title: '报告人' },
  { code: 'priority', title: '优先级' },
  { code: 'remainingTime', title: '预估时间' },
  { code: 'storyPoints', title: '故事点' },
  { code: 'linkIssue', title: '关联问题' },
  { code: 'version', title: '版本' },
  { code: 'label', title: '标签' },
  { code: 'estimatedStartTime', title: '预计开始时间' },
  { code: 'estimatedEndTime', title: '预计结束时间' },
];

interface Props {
  importFieldsRef: React.MutableRefObject<{fields: string[]}>,
}

const ImportFields: React.FC<Props> = ({ importFieldsRef }) => {
  const { isInProgram } = useIsInProgram();
  const [updateCount, setUpdateCount] = useState<number>(0);
  const fieldsOptionDataSet = useMemo(() => new DataSet({ paging: false }), []);

  const applyType = getApplyType();
  let requiredFields: string[] = useMemo(() => [], []);
  let systemFields: {code: string, title: string}[] = useMemo(() => [], []);

  if (applyType === 'program') {
    systemFields = programSystemFields;
    requiredFields = programImportRequiresFields;
  } else if (isInProgram) {
    systemFields = projectSystemFields;
    requiredFields = projectImportRequiresFields;
  } else {
    systemFields = subProjectSystemFields;
    requiredFields = subProjectImportRequiredFields;
  }

  const chooseDataSet = useMemo(() => new DataSet({
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
      update: () => {
        setUpdateCount((count) => count + 1);
      },
    },
  }), [fieldsOptionDataSet, requiredFields]);

  useEffect(() => {
    const loadData = async () => {
      const fields = await fieldApi.getFoundationHeader();
      fieldsOptionDataSet.loadData([...systemFields, ...fields]);
    };
    loadData();
  }, [chooseDataSet, fieldsOptionDataSet, systemFields]);

  useImperativeHandle(importFieldsRef, () => {
    console.log('useImperativeHandle：');
    console.log(chooseDataSet?.current?.get('fields'));
    return ({
      fields: chooseDataSet?.current?.get('fields') || requiredFields,
    });
  });

  return (
    <div className={styles.importFields}>
      <div className={styles.importFields_title}>选择字段</div>
      <div className={styles.importFields_content}>
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
