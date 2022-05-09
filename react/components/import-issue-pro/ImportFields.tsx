/* eslint-disable react/require-default-props */
import React, {
  useMemo, useEffect, useImperativeHandle, useState, useCallback, useRef, forwardRef,
} from 'react';
import { useObserver } from 'mobx-react-lite';
import {
  SelectBox, DataSet, Button, TextField, Icon,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { FuncType } from 'choerodon-ui/pro/lib/button/interface';
import { includes, noop } from 'lodash';
import {
  useCreation, useToggle, useUpdateEffect,
} from 'ahooks';
import { fieldApi } from '@/api';
import useIsInProgram from '@/hooks/useIsInProgram';
import styles from './ImportFields.less';
import { ImportIssueContextProps } from './stores';

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
export interface IIortIssueFieldsRef {
  setValue: (codes: string[]) => void
}
export interface IImportIssueFieldsEvents {
  /**
* 值更新事件
* @param 是否是初始化值
*/
  onUpdate?: (filterCodes: string[], init: boolean) => void
  /**
   * 选项加载完成事件
   */
  onOptionLoad?: (allFields: Array<{ code: string, title: string, system: boolean }>) => void
  /**
   * 必填选项加载完成事件
   */
  onRequiredFieldLoad?: (requiredFieldCodes: string[]) => void
}
interface IImportIssueFieldsProps extends Pick<ImportIssueContextProps, 'requires' | 'systems' | 'fields' | 'applyType'> {
  events?: IImportIssueFieldsEvents
}

const ImportFields = forwardRef<IIortIssueFieldsRef, IImportIssueFieldsProps>(({
  applyType, requires, systems, fields: fs, events: propsEvents,
}, ref) => {
  const fieldFormItemRef = useRef<SelectBox>(null);
  const { isInProgram, loading } = useIsInProgram();
  const [requiredFields, setRequiredFields] = useState<string[]>(() => requires || []);
  const [btnStatus, { toggle: toggleBtnStatus }] = useToggle('NONE', 'ALL');
  const [systemFields, setSystemFields] = useState<{ code: string, title: string }[]>(systems || []);
  const [allFields, setAllFields] = useState<{ code: string, title: string, system: boolean }[]>([]);
  const events = useCreation(() => ({ ...propsEvents }) as Required<NonNullable<IImportIssueFieldsProps['events']>>, []);

  events.onUpdate = propsEvents?.onUpdate || noop;
  events.onOptionLoad = propsEvents?.onOptionLoad || noop;
  events.onRequiredFieldLoad = propsEvents?.onRequiredFieldLoad || noop;

  const fieldsOptionDataSet = useMemo(() => new DataSet({
    paging: false,
  }), []);

  const chooseDataSet = useMemo(() => {
    let firstUpdate = true;
    return new DataSet({
      autoCreate: true,
      autoQuery: false,
      fields: [{
        name: 'fields',
        type: 'string' as FieldType,
        textField: 'title',
        valueField: 'code',
        multiple: true,
        options: fieldsOptionDataSet,
      }],
      events: {
        update: ({ value }: { value: string[] }) => {
          events.onUpdate((value || []).filter((code) => !['linkIssue', 'parentIssue'].includes(code)), firstUpdate);
          firstUpdate = false;
        },
      },
    });
  }, [events, fieldsOptionDataSet]);

  useEffect(() => {
    if (!systems && !requires) {
      if (!loading) {
        const newFieldConfig = { required: [] as any[], system: [] as any[] };
        if (applyType === 'program') {
          Object.assign(newFieldConfig, {
            required: programImportRequiresFields,
            system: programSystemFields,
          });
        } else if (isInProgram) {
          Object.assign(newFieldConfig, {
            required: subProjectImportRequiredFields,
            system: subProjectSystemFields,
          });
        } else {
          Object.assign(newFieldConfig, {
            required: projectImportRequiresFields,
            system: projectSystemFields,
          });
        }
        // 设置必填值
        chooseDataSet.current?.set('fields', newFieldConfig.required);
        setRequiredFields(newFieldConfig.required);
        setSystemFields(newFieldConfig.system);
        events.onRequiredFieldLoad(newFieldConfig.required);
      }
    }
  }, [applyType, chooseDataSet, events, isInProgram, loading, requires, systems]);
  useEffect(() => {
    const loadData = async () => {
      const fields = fs || await fieldApi.getFoundationHeader(applyType === 'program' ? 'programIssueType' : 'agileIssueType');
      const allFs = [...(systemFields.map((item) => ({ ...item, system: true }))), ...fields];
      setAllFields(allFs);
      events.onOptionLoad(allFs);
      fieldsOptionDataSet.loadData(allFs);
    };

    if ((systemFields && systemFields.length) || fs?.length) {
      loadData();
    }
  }, [applyType, events, fieldsOptionDataSet, fs, systemFields]);
  const setValue = useCallback((codes: string[]) => {
    chooseDataSet.current?.init('fields', codes);
  }, [chooseDataSet]);
  useImperativeHandle(ref, () => ({ setValue }));

  useUpdateEffect(() => {
    if (btnStatus === 'ALL') {
      fieldFormItemRef.current?.chooseAll();
    } else {
      fieldFormItemRef.current?.chooseRe();
    }
  }, [btnStatus]);
  const handleSearch = useCallback((value) => {
    // @ts-ignore
    fieldsOptionDataSet.loadData(allFields.filter((item) => item.title.indexOf(value || '') > -1));
  }, [allFields, fieldsOptionDataSet]);

  return useObserver(() => (
    <div className={styles.importFields}>
      <div className={styles.importFields_title}>
        <span>选择字段</span>
        <Button funcType={'flat' as FuncType} className={styles.importFields_btn} onClick={() => toggleBtnStatus()}>{btnStatus === 'NONE' ? '全选' : '全不选'}</Button>
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
          ref={fieldFormItemRef}
          dataSet={chooseDataSet}
          checkValueOnOptionsChange={false}
          name="fields"
          onOption={({ record }) => ({
            disabled: includes(requiredFields, record.get('code')),
          })}
        />
      </div>
    </div>
  ));
});

export default ImportFields;
