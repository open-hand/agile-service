import React, {
  useState, useEffect, useRef, useCallback, useMemo,
} from 'react';

import {
  Form, DataSet, Select, Button, Row, Col,
} from 'choerodon-ui/pro';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { ModalProps } from 'choerodon-ui/pro/lib/modal/Modal';
import { Divider } from 'choerodon-ui';
import { axios, Choerodon } from '@choerodon/boot';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import DataSetField from 'choerodon-ui/pro/lib/data-set/Field';
import useFields from '@/routes/Issue/components/BatchModal/useFields';
import { getProjectId } from '@/utils/common';
import { User } from '@/common/types';
import { fieldApi, pageRuleApi } from '@/api';
import Loading from '@/components/Loading';
import { find, map } from 'lodash';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import SelectUser from '@/components/select/select-user';
import moment from 'moment';
import renderRule, {
  IField, Operation, IFieldWithType, IMiddleFieldType,
} from './renderRule';
import styles from './index.less';

const { Option } = Select;

interface IModalProps extends ModalProps { // pro 组件Modal 注入的modal
    handleOk: (fn:() => Promise<boolean>) => void,
    handleCancel: (promise: () => boolean) => boolean,
    close: (destroy?: boolean) => void,
    update: (modalProps: ModalProps) => void
}

interface Props {
    modal?: IModalProps,
    ruleTableDataSet: DataSet
    ruleId?: string
}

interface Express {
  fieldCode: string,
  operation: Operation,
  relationshipWithPervious: 'and' | 'or',
  // text,input
  valueStr?: string, //
  // 单选，member
  valueId?: string,
  // 多选
  valueIdList?: string[],
  // number整数,需要判断是否允许小数
  valueNum?: number,
  // number有小数， 需要判断是否允许小数
  valueDecimal?: number,
  // date,datetime
  valueDate?: string,
  // time
  valueDateHms?: string,
  predefined?: boolean,
  fieldType?: IMiddleFieldType,
  // 是否允许小数，需要判断是否允许小数
  allowDecimals?: boolean,
}
// 'in' | 'not_in' | 'is' | 'is_not' | 'eq' | 'not_eq' | 'gt' | 'gte' | 'lt' | 'lte' | 'like' | 'not_like'
const operationMap = new Map([
  ['in', '包含'],
  ['not_in', '不包含'],
  ['is', '是'],
  ['is_not', '不是'],
  ['eq', '等于'],
  ['not_eq', '不等于'],
  ['gt', '大于'],
  ['gte', '大于或等于'],
  ['lt', '小于'],
  ['lte', '小于或等于'],
  ['like', '包含'],
  ['not_like', '不包含'],
]);

const systemFieldTypeMap = new Map([
  ['assignee', 'member'],
  ['component', 'multiple'],
  ['creation_date', 'datetime'],
  ['epic', 'single'],
  ['estimated_end_time', 'datetime'],
  ['estimated_start_time', 'datetime'],
  ['fix_version', 'multiple'],
  ['influence_version', 'multiple'],
  ['issue_type', 'single'],
  ['label', 'multiple'],
  ['last_update_date', 'datetime'],
  ['priority', 'single'],
  ['remain_time', 'number'],
  ['reporter', 'single'],
  ['sprint', 'single'],
  ['status', 'single'],
  ['story_point', 'number'],
]);

const customTypeMap = new Map([
  ['radio', 'option'],
  ['checkbox', 'option'],
  ['time', 'date_hms'],
  ['datetime', 'date'],
  ['number', 'number'],
  ['input', 'string'],
  ['text', 'text'],
  ['single', 'option'],
  ['multiple', 'option'],
  ['member', 'option'],
  ['date', 'date'],
]);

const aoMap = new Map([
  ['or', '或'],
  ['and', '且'],
]);

const excludeCode = ['summary', 'description', 'epicName', 'timeTrace', 'belongToBacklog', 'progressFeedback', 'email'];

const formatMoment = (type: 'date' | 'datetime' | 'time', d: string) => {
  switch (type) {
    case 'date': {
      return `${moment(d).format('YYYY-MM-DD')} 00:00:00`;
    }
    case 'datetime': {
      return moment(d).format('YYYY-MM-DD HH:mm:ss');
    }
    case 'time': {
      return moment(d).format('HH:mm:ss');
    }
  }
};

const RuleModal: React.FC<Props> = ({ modal, ruleTableDataSet, ruleId }) => {
  const formRef: React.MutableRefObject<Form | undefined> = useRef();
  const [fieldData, setFieldData] = useState<IFieldWithType[]>([]);
  const fieldDataRef = useRef<IFieldWithType[]>([]);
  const [fields, Field] = useFields();
  const [updateCount, setUpdateCount] = useState<number>(0);
  const systemDataRefMap = useRef<Map<string, any>>(new Map());
  const [initRule, setInitRule] = useState({});
  const [loading, setLoading] = useState<boolean>(false);

  const modalDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'receiverList',
      required: true,
    }],
    events: {
      update: ({
        // @ts-ignore
        dataSet, record, name, value, oldValue,
      }) => {
        const key = name.split('-')[0];
        if (name.indexOf('code') > -1) {
          const field = (fieldDataRef?.current || []).find((item) => item.code === value);
          if (field) {
            const {
              system, extraConfig, code, fieldType,
            } = field;
            if (fieldType === 'number') {
              const valueField = dataSet.current.getField(`${key}-value`);
              if (system && (code === 'story_point' || code === 'remain_time')) {
                valueField.set('max', 100);
                valueField.set('min', 0);
                valueField.set('step', 0.1);
                valueField.set('validator', (numberValue: number) => {
                  if (numberValue && /(^\d{1,3}\.{1}\d{1}$)|(^[1-9]\d{0,2}$)/.test(numberValue.toString())) {
                    return true;
                  }
                  return '请输入小于3位的整数或者整数位小于3位小数点后一位的小数';
                });
              } else if (extraConfig) {
                valueField.set('step', 0.01);
                valueField.set('validator', (numberValue: number) => {
                  if (numberValue && /(^-?[0-9]+$)|(^[-]?[0-9]+(\.[0-9]{1,2})?$)/.test(numberValue.toString())) {
                    return true;
                  }
                  return '请输入整数或者小数点后一位或两位的小数';
                });
              } else {
                valueField.set('step', 1);
              }
            }
          }
          dataSet.current.set(`${key}-operation`, undefined);
          dataSet.current.set(`${key}-value`, undefined);
        }
        if (name.indexOf('operation') > -1) {
          dataSet.current.set(`${key}-value`, undefined);
        }
        setUpdateCount((count) => count + 1);
      },
    },
  }), [fieldDataRef]);

  const renderOperations = useCallback((fieldK: { key: number }) => {
    const { key } = fieldK;
    const code = modalDataSet?.current?.get(`${key}-code`);
    const field = fieldData.find((item: IFieldWithType) => item.code === code);
    if (field) {
      const { fieldType } = field;
      let operations: {value: Operation, operation: string}[] = [];
      switch (fieldType) {
        case 'checkbox':
        case 'multiple': {
          operations = [
            { value: 'in', operation: '包含' },
            { value: 'not_in', operation: '不包含' },
            { value: 'is', operation: '是' },
            { value: 'is_not', operation: '不是' },
          ];
          break;
        }
        case 'radio':
        case 'single': {
          operations = [
            { value: 'eq', operation: '等于' },
            { value: 'not_eq', operation: '不等于' },
            { value: 'is', operation: '是' },
            { value: 'is_not', operation: '不是' },
          ];
          break;
        }
        case 'member': {
          operations = [
            { value: 'eq', operation: '等于' },
            { value: 'not_eq', operation: '不等于' },
            { value: 'is', operation: '是' },
            { value: 'is_not', operation: '不是' },
          ];
          break;
        }
        case 'text':
        case 'input': {
          operations = [
            { value: 'like', operation: '包含' },
            { value: 'not_like', operation: '不包含' },
            { value: 'eq', operation: '等于' },
            { value: 'not_eq', operation: '不等于' },
          ];
          break;
        }
        case 'number': {
          operations = [
            { value: 'gt', operation: '大于' },
            { value: 'gte', operation: '大于或等于' },
            { value: 'lt', operation: '小于' },
            { value: 'lte', operation: '小于或等于' },
            { value: 'eq', operation: '等于' },
            { value: 'is', operation: '是' },
            { value: 'is_not', operation: '不是' },
          ];
          break;
        }
        case 'time':
        case 'datetime':
        case 'date': {
          operations = [
            { value: 'gt', operation: '大于' },
            { value: 'gte', operation: '大于或等于' },
            { value: 'lt', operation: '小于' },
            { value: 'lte', operation: '小于或等于' },
            { value: 'eq', operation: '等于' },
          ];
          break;
        }
      }
      return (
        <Select clearButton={false} required name={`${key}-operation`} label="关系">
          {
              operations.map((item) => (
                <Option key={`${key}-${item.value}`} value={item.value}>{item.operation}</Option>
              ))
          }
        </Select>
      );
    }
    return (
      <Select name={`${key}-operation`} required label="关系" />
    );
  }, [fieldData, modalDataSet]);

  const addField = useCallback((name, props) => {
    const field = new DataSetField({ ...props, name }, modalDataSet, modalDataSet.current);
    modalDataSet?.current?.fields.set(name, field);
  }, [modalDataSet]);

  const removeField = useCallback((name) => {
    modalDataSet?.fields?.delete(name);
    modalDataSet?.current?.fields.delete(name);
  }, [modalDataSet]);

  const addFieldRule = useCallback((key, i) => {
    addField(`${key}-code`, {
      required: true,
    });
    addField(`${key}-operation`, {
      required: true,
    });
    addField(`${key}-value`, {
      required: true,
    });
    if (i > 0) {
      addField(`${key}-ao`, {
        required: true,
      });
    }
  }, [addField]);

  useEffect(() => {
    if (!ruleId) {
      const newKey = Field.add();
      addFieldRule(newKey, 0);
    }
    setLoading(true);
    Promise.all([pageRuleApi.getPageRuleSystemFields(), fieldApi.getCustomFields()]).then(([systemFields, customFields]) => {
      const transformedSystemFields = systemFields.map((item: { fieldCode: string; }) => ({
        ...item,
        code: item.fieldCode,
        fieldType: systemFieldTypeMap.get(item.fieldCode),
        system: true,
      }));
      const transformedCustomFields = customFields.map((item: IField) => ({
        ...item,
        type: customTypeMap.get(item.fieldType),
        system: false,
      }));
      const data = [...transformedSystemFields, ...transformedCustomFields].filter((item) => !find(excludeCode, (code) => code === item.code));
      fieldDataRef.current = data;
      batchedUpdates(() => {
        setFieldData(data);
        setLoading(false);
      });
    });
  }, [addFieldRule, ruleId]);

  const getFieldValue = useCallback((name) => {
    const { current } = modalDataSet;
    if (current) {
      return current.get(name);
    }
    return '';
  }, [modalDataSet]);

  const transformValue = useCallback((fieldInfo: IFieldWithType, operation: Operation, value: any) => {
    const {
      fieldType, system, fieldOptions, code,
    } = fieldInfo;
    if (operation === 'is' || operation === 'is_not') {
      return '空';
    }
    if (system) {
      const options = systemDataRefMap.current.get(code);
      switch (code) {
        case 'priority':
        case 'status': {
          const selectOption = find(options, { id: value });
          return selectOption?.name;
        }
        case 'issue_type': {
          const selectOption = find(options, { typeCode: value });
          return selectOption?.name;
        }
        case 'component': {
          const selectOptions = options.filter((option: { componentId: string; }) => value.indexOf(option.componentId) > -1);
          return `[${map(selectOptions, 'name').join(',')}]`;
        }
        case 'label': {
          const selectOptions = options.filter((option: { labelId: string; }) => value.indexOf(option.labelId) > -1);
          return `[${map(selectOptions, 'labelName').join(',')}]`;
        }
        case 'influence_version':
        case 'fix_version': {
          const selectOptions = options.filter((option: { versionId: string; }) => value.indexOf(option.versionId) > -1);
          return `[${map(selectOptions, 'name').join(',')}]`;
        }
        case 'epic': {
          const selectOption = find(options, { issueId: value });
          return selectOption?.epicName;
        }
        case 'sprint': {
          const selectOption = find(options, { sprintId: value });
          return selectOption?.sprintName;
        }
        case 'reporter':
        case 'assignee': {
          const selectOption = find(options, { id: value });
          return selectOption?.realName;
        }
      }
    }
    switch (fieldType) {
      case 'multiple':
      case 'checkbox': {
        const selectOptions = fieldOptions?.filter((option) => value.indexOf(option.id) > -1);
        return `[${map(selectOptions, 'value').join(',')}]`;
      }
      case 'radio':
      case 'single': {
        const selectOption = find(fieldOptions, { id: value });
        return selectOption?.value;
      }
      case 'member': {
        const memberOptions = systemDataRefMap.current.get(code);
        const selectMembers = find(memberOptions, { id: value });
        return selectMembers?.realName;
      }
      case 'number':
      case 'text':
      case 'input': {
        return value;
      }
      case 'time':
      case 'datetime':
      case 'date': {
        return formatMoment(fieldType as 'time' | 'datetime' | 'date', value);
      }
      default:
        return value;
    }
  }, []);

  const transformSumitData = useCallback(() => {
    let expressQuery = '';
    const expressList: Express[] = [];
    const codeValues:{name:string, value:any}[] = [];
    const submitData: {name: string, value: any}[] = [];

    fields.forEach(({ key }: { key: number}, i: number) => {
      submitData.push({
        name: `${key}-code`,
        value: getFieldValue(`${key}-code`),
      }, {
        name: `${key}-operation`,
        value: getFieldValue(`${key}-operation`),
      }, {
        name: `${key}-value`,
        value: getFieldValue(`${key}-value`),
      });
      if (i > 0) {
        submitData.push({
          name: `${key}-ao`,
          value: getFieldValue(`${key}-ao`),
        });
      }
    });

    submitData.forEach(({ name, value }) => {
      if (name.split('-')[1] === 'code' && fieldData.find((field) => field.code === value)) {
        codeValues.push({
          name,
          value,
        });
      }
    });

    codeValues.forEach((codeField) => {
      const key = codeField.name?.split('-')[0];
      if (key) {
        const fieldInfo = fieldData.find((item) => item.code === codeField.value);
        if (fieldInfo) {
          const {
            fieldType, type, system, name, extraConfig, code,
          } = fieldInfo;
          const valueIsNull = getFieldValue(`${key}-operation`) === 'is' || getFieldValue(`${key}-operation`) === 'is_not';
          const value = toJS(getFieldValue(`${key}-value`));
          const hasAo = submitData.find(({ name: fieldName }) => fieldName === `${key}-ao`);
          if (value || value === 0 || valueIsNull) {
            expressList.push({
              fieldCode: code,
              operation: getFieldValue(`${key}-operation`),
              relationshipWithPervious: hasAo && getFieldValue(`${key}-ao`),
              // text,input
              valueStr: (fieldType === 'input' || fieldType === 'text' || code === 'issue_type') && !valueIsNull ? value : undefined,
              // 单选，member
              valueId: (fieldType === 'single' || fieldType === 'member' || fieldType === 'radio') && code !== 'issue_type' && !valueIsNull ? value : undefined,
              // 多选
              valueIdList: (fieldType === 'multiple' || fieldType === 'checkbox') && !valueIsNull ? value : undefined,
              // number整数,需要判断是否允许小数
              valueNum: fieldType === 'number' && !extraConfig && !valueIsNull && code !== 'remain_time' && code !== 'story_point' ? value : undefined,
              // number有小数， 需要判断是否允许小数
              valueDecimal: fieldType === 'number' && (extraConfig || code === 'remain_time' || code === 'story_point') && !valueIsNull ? value : undefined,
              // date,datetime
              valueDate: (fieldType === 'date' || fieldType === 'datetime') && !valueIsNull ? formatMoment(fieldType, value) : undefined,
              // time
              valueDateHms: fieldType === 'time' && !valueIsNull ? formatMoment(fieldType, value) : undefined,
              predefined: system,
              fieldType: type,
              // 是否允许小数，需要判断是否允许小数
              allowDecimals: fieldType === 'number' && !valueIsNull ? extraConfig : undefined,
            });
            const ao = hasAo && getFieldValue(`${key}-ao`) && aoMap.get(getFieldValue(`${key}-ao`));
            expressQuery += `${ao ? `${ao} ` : ''}${name} ${operationMap.get(getFieldValue(`${key}-operation`))} ${transformValue(fieldInfo, getFieldValue(`${key}-operation`), getFieldValue(`${key}-value`))} `;
          }
        }
      }
    });
    return {
      expressQuery,
      expressList,
    };
  }, [fieldData, getFieldValue, modalDataSet, transformValue, fields]);

  const handleClickSubmit = useCallback(async () => {
    if (await modalDataSet.validate()) {
      const expressObj = transformSumitData();
      const data = {
        ...initRule,
        receiverList: (toJS(getFieldValue('receiverList')) || []).map((id: string) => ({ id })),
        ccList: (toJS(getFieldValue('ccList')) || []).map((id: string) => ({ id })),
        ...expressObj,
      };
      if (!ruleId) {
        return pageRuleApi.create(data).then(() => {
          Choerodon.prompt('创建成功');
          ruleTableDataSet.query();
          return true;
        }).catch(() => {
          Choerodon.prompt('创建失败');
          return false;
        });
      }
      return pageRuleApi.update(ruleId, data).then(() => {
        Choerodon.prompt('编辑成功');
        ruleTableDataSet.query(ruleTableDataSet.currentPage);
        return true;
      }).catch(() => {
        Choerodon.prompt('编辑失败');
        return false;
      });
    }
    return false;
  }, [modalDataSet, ruleId, ruleTableDataSet, transformSumitData, initRule]);

  useEffect(() => {
    modal?.handleOk(handleClickSubmit);
  }, [handleClickSubmit, modal]);

  const setFieldValue = useCallback((name: string, value: any) => {
    const { current } = modalDataSet;
    if (current) {
      current.set(name, value);
    }
  }, [modalDataSet]);

  useEffect(() => {
    if (ruleId && fieldData.length) {
      setLoading(true);
      // @ts-ignore
      pageRuleApi.getRule(ruleId).then((res) => {
        batchedUpdates(() => {
          const { ccList = [], receiverList = [], expressList = [] } = res;
          setFieldValue('ccList', ccList.map((item: User) => item.id));
          setFieldValue('receiverList', receiverList.map((item: User) => item.id));
          const existFields = fieldData.filter((item: IFieldWithType) => find(expressList, { fieldCode: item.code }));
          const initFields = Field.init(new Array(existFields.length).fill({}));
          initFields.forEach((item: { key: number }, i: number) => {
            addFieldRule(item.key, i);
          });
          expressList.forEach((item: Express, i: number) => {
            const {
              fieldCode, relationshipWithPervious, operation, valueStr, valueId, valueIdList, valueNum, valueDecimal, valueDate, valueDateHms,
            } = item;
            const field = find(fieldData, { code: fieldCode });
            const fieldValue = valueStr || valueId || valueIdList || valueNum || valueDecimal || valueDate || valueDateHms;
            if (field) {
              const { fieldType, code } = field;
              const { key } = initFields[i];
              setFieldValue(`${key}-code`, fieldCode);
              setFieldValue(`${key}-operation`, operation);
              if (operation !== 'is' && operation !== 'is_not') {
                setFieldValue(`${key}-value`, fieldType === 'date' || fieldType === 'datetime' || fieldType === 'time' ? moment(fieldType === 'time' ? `${moment().format('YYYY-MM-DD')} ${fieldValue}` : fieldValue) : fieldValue);
              } else {
                setFieldValue(`${key}-value`, 'empty');
              }
              if (relationshipWithPervious) {
                setFieldValue(`${key}-ao`, relationshipWithPervious);
              }
            }
          });
          setInitRule(res);
          setLoading(false);
        });
      }).catch((e: ErrorEvent) => {
        setLoading(false);
      });
    }
  }, [ruleId, fieldData, setFieldValue, addFieldRule]);

  const existsFieldCodes: string[] = [];
  fields.forEach((fieldWithKey: { key: number }) => {
    const { key } = fieldWithKey;
    const keyCode = modalDataSet?.current?.get(`${key}-code`);
    const field = fieldData.find((item) => item.code === keyCode);
    if (field) {
      existsFieldCodes.push(field.code);
    }
  });

  return (
    <div className={styles.rule_form}>
      <Loading loading={loading} />
      <Form dataSet={modalDataSet} ref={formRef as React.RefObject<Form>}>
        <div className={`${styles.rule_form_setting} ${styles.rule_form_objectSetting}`}>
          <p className={styles.rule_form_setting_title}>通知对象设置</p>
          <SelectUser
            required
            style={{
              width: 600,
            }}
            // @ts-ignore
            autoQueryConfig={{
              selectedUserIds: getFieldValue('receiverList'),
            }}
            name="receiverList"
            label="通知对象"
            multiple
            maxTagCount={6}
            maxTagTextLength={4}
          />
          <SelectUser
            style={{
              width: 600,
              marginTop: 27,
            }}
            // @ts-ignore
            autoQueryConfig={{
              selectedUserIds: getFieldValue('ccList'),
            }}
            name="ccList"
            label="抄送人"
            multiple
            maxTagCount={6}
            maxTagTextLength={4}
          />
        </div>
        <Divider />
        <div className={`${styles.rule_form_setting} ${styles.rule_form_ruleSetting}`}>
          <p className={styles.rule_form_setting_title}>通知规则设置</p>
          {
                fields.map((f: { key: number }, i: number, arr: { key: number }[]) => {
                  const { key } = f;
                  return (
                    <Row
                      key={key}
                      gutter={20}
                      style={{
                        marginBottom: 15,
                      }}
                    >
                      <Col span={10}>
                        <Row gutter={20}>
                          {
                            i !== 0 && (
                              <Col span={8}>
                                <Select
                                  required
                                  label="关系"
                                  name={`${key}-ao`}
                                  clearButton={false}
                                >
                                  <Option value="and">且</Option>
                                  <Option value="or">或</Option>
                                </Select>
                              </Col>

                            )
                          }
                          <Col span={i !== 0 ? 16 : 24}>
                            <Select
                              style={{
                                width: '100%',
                              }}
                              label="属性"
                              name={`${key}-code`}
                              clearButton={false}
                            >
                              {
                                      fieldData.filter((field: IFieldWithType) => (
                                        modalDataSet?.current?.get(`${key}-code`) === field.code
                                      ) || !existsFieldCodes.find((code: string) => code === field.code)).map((field:IFieldWithType) => (
                                        <Option value={field.code}>
                                          {field.name}
                                        </Option>
                                      ))
                              }
                            </Select>
                          </Col>
                        </Row>
                      </Col>
                      <Col span={4}>
                        {renderOperations(f)}
                      </Col>
                      <Col span={8}>
                        {
                          renderRule(modalDataSet, f, fieldData, systemDataRefMap, getFieldValue)
                        }
                      </Col>
                      <Col span={2}>
                        <Button
                          disabled={arr.length === 1 && i === 0}
                          onClick={() => {
                            // @ts-ignore
                            Field.remove(key);
                            removeField(`${key}-code`);
                            removeField(`${key}-operation`);
                            removeField(`${key}-value`);
                            removeField(`${key}-ao`);
                            if (i === 0) {
                              if (fields[i + 1]) {
                                removeField(`${fields[i + 1].key}-ao`);
                              }
                            }
                          }}
                          icon="delete"
                        />
                      </Col>
                    </Row>
                  );
                })
               }
          <div>
            <Button
                // @ts-ignore
              onClick={() => {
                const newKey = Field.add();
                addFieldRule(newKey, fields.length);
              }}
              icon="add"
              color={'blue' as ButtonColor}
            >
              添加字段
            </Button>
          </div>
        </div>
      </Form>
    </div>

  );
};

export default observer(RuleModal);
