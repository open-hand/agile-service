import React, {
  ReactElement, useCallback, useState, useEffect, useRef,
} from 'react';
import { Modal, Form, DataSet } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { includes, map, find } from 'lodash';
import { toJS } from 'mobx';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { EmptyPage } from '@choerodon/components';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { OldLoading as Loading } from '@/components/Loading';
import { IField, IModalProps } from '@/common/types';
import noData from '@/assets/image/NoData.svg';
import { ICascadeRule, pageConfigApi } from '@/api';
import LINK_URL from '@/constants/LINK_URL';
import to from '@/utils/to';
import styles from './Linkage.less';
import FieldOptions from './components/field-options';
import ChosenFields from './components/chosen-fields';
import Rule from './components/Rule';
import { DATETIME, FORMAT_FIELDS } from '@/constants/DATE_FORMAT';
import { formatMinute } from '@/utils/formatDate';

interface ColumnProps {
  title: string | ReactElement,
  children: ReactElement | ReactElement[] | null | Array<ReactElement | null>,
  width?: number
  columnStyle?: React.CSSProperties
  contentStyle?: React.CSSProperties
  bordered?: boolean
}

const LinkageColumn:React.FC<ColumnProps> = ({
  width, title, children, columnStyle, contentStyle, bordered = true, ...otherProps
}) => (
  <div
    className={styles.linkage_column}
    style={{
      width,
      borderRight: bordered ? '1px solid var(--divider)' : 'none',
      ...(columnStyle || {}),
    }}
    {...otherProps}
  >
    <div className={styles.title}>{title}</div>
    <div className={styles.content} style={{ ...(contentStyle || {}) }}>{children}</div>
  </div>
);

interface Props {
  field: {
    id: string,
    name: string,
    fieldCode: string,
    system: boolean
  }
  modal?: IModalProps
  issueTypeId: string
  onOk: () => void
}

export interface ICascadeLinkageSetting {
  id?: string
  chosenField: IField
  fieldRelOptionList?: {meaning: string, value: string}[]
  defaultValue?: any
  hidden?: boolean
  required?: boolean
}

export interface ICascadeLinkage {
  objectVersionNumber: number,
  id: string,
  issueTypeId: string,
  fieldId: string,
  fieldCode: string,
  fieldName: string,
  fieldType: string,
  fieldSystem: boolean,
  fieldOptionId: string,
  cascadeFieldId: string,
  hidden: boolean,
  required: boolean,
  cascadeFieldName: string
  cascadeFieldCode: string,
  cascadeFieldType: string,
  cascadeFieldSystem: boolean,
  defaultValue: any,
  defaultValueObjs: any[],
  defaultIds: any[], // Select、人员(单选、多选)
  fieldCascadeRuleOptionList: any
}

const selectTypes = ['radio', 'checkbox', 'single', 'multiple', 'member', 'multiMember'];
const singleSelectTypes = ['radio', 'single', 'member'];

const Linkage: React.FC<Props> = ({
  field, issueTypeId, modal, onOk,
}) => {
  const [loading, setLoading] = useState<boolean>(false);
  const [currentOptionId, setCurrentOptionId] = useState<string | undefined>(undefined);
  const [linkagesMap, setLinkagesMap] = useState<Map<string, ICascadeLinkageSetting[]>>(new Map());
  const [dataSet, setDataSet] = useState<DataSet>();
  const [currentSelected, setCurrentSelected] = useState<string | undefined>();
  const [cascadeRuleList, setCascadeRuleList] = useState<ICascadeRule[]>([]);
  const [hasOptions, setHasOptions] = useState<boolean>(true);

  const formatDate = useCallback(({ fieldCode, defaultValue, format }) => {
    if (fieldCode && defaultValue && includes(FORMAT_FIELDS, fieldCode)) {
      return formatMinute({ value: defaultValue, format });
    }
    return defaultValue;
  }, []);

  useEffect(() => {
    const getCascadeRuleList = async () => {
      setLoading(true);
      const res: ICascadeLinkage[] = await pageConfigApi.getCascadeRuleList(issueTypeId, field.id);
      batchedUpdates(() => {
        // @ts-ignore
        setCascadeRuleList(res);
        const cascadeLinkageMap = new Map();
        res.forEach((item) => {
          if (!cascadeLinkageMap.has(item.fieldOptionId)) {
            cascadeLinkageMap.set(item.fieldOptionId, []);
          }
          cascadeLinkageMap.get(item.fieldOptionId).push({
            id: item.id,
            chosenField: {
              name: item.cascadeFieldName,
              id: item.cascadeFieldId,
              code: item.cascadeFieldId,
              system: item.cascadeFieldSystem,
              fieldType: item.cascadeFieldType,
              fieldCode: item.cascadeFieldCode,
            },
            fieldRelOptionList: (item.fieldCascadeRuleOptionList || [])?.map((option: { cascadeOptionId: string }) => ({ value: option.cascadeOptionId })),
            // eslint-disable-next-line no-nested-ternary
            defaultValue: formatDate({ fieldCode: item.cascadeFieldCode, defaultValue: item.defaultValue }) || (includes(singleSelectTypes, item.cascadeFieldType) ? item.defaultIds && item.defaultIds[0] : (item.defaultIds?.length ? item.defaultIds : undefined)),
            hidden: item.hidden,
            required: item.required,
          });
        });
        setLinkagesMap(cascadeLinkageMap);
        setLoading(false);
      });
    };
    getCascadeRuleList();
  }, [field.id, issueTypeId]);

  const switchOption = useCallback((id: string) => {
    setCurrentOptionId(id);
    setCurrentSelected(undefined);
    setDataSet(new DataSet({
      fields: [{
        name: 'chosenField',
        label: '被关联字段',
      }, {
        name: 'fieldRelOptionList',
        label: '可见选项',
        multiple: true,
        type: 'object' as any,
      }, {
        name: 'defaultValue',
        label: '默认值',
      }, {
        name: 'hidden',
        label: '隐藏字段',
        dynamicProps: {
          disabled: ({ record }) => record.get('required'),
        },
      }, {
        name: 'required',
        label: '设置为必填字段',
        dynamicProps: {
          disabled: ({ record }) => record.get('hidden'),
        },
      }],
    }));
  }, []);
  const prepareData = useCallback(async () => {
    const hasValue = dataSet?.find((r) => r.get('chosenField')?.id);
    if (hasValue && !await dataSet?.validate()) {
      return false;
    }
    const data: any = dataSet?.toData();
    linkagesMap.set(currentOptionId as string, hasValue ? data : []);
    return true;
  }, [currentOptionId, dataSet, linkagesMap]);

  useEffect(() => {
    const getFieldCascadeRuleOptionList = (fieldType: string, fieldRelOptionList: {meaning: string, value: string}[] | undefined, defaultValue: any) => {
      if (includes(['radio', 'checkbox', 'single', 'multiple'], fieldType) && fieldRelOptionList?.length) {
        return fieldRelOptionList.map((option) => ({
          cascadeOptionId: option.value,
          defaultOption: Array.isArray(defaultValue) ? includes(defaultValue, option.value) : option.value === defaultValue,
        }));
      }
      if (fieldType === 'member' && defaultValue) {
        return ([{
          cascadeOptionId: defaultValue,
          defaultOption: true,
        }]);
      }
      if (fieldType === 'multiMember' && defaultValue?.length) {
        return defaultValue.map((value: string) => ({
          cascadeOptionId: value,
          defaultOption: true,
        }));
      }
      return undefined;
    };
    const handleOk = async () => {
      if (!await prepareData()) {
        return false;
      }
      const cascadeRuleData: ICascadeRule[] = [];
      for (const [key, values] of linkagesMap.entries()) {
        values.forEach((value) => {
          const {
            chosenField, defaultValue, fieldRelOptionList, required, hidden,
          } = value;
          const originCascadeRule = cascadeRuleList.find((item) => item.fieldOptionId === key && item.cascadeFieldId === chosenField?.id) || { id: undefined };
          cascadeRuleData.push({
            ...originCascadeRule,
            _status: !originCascadeRule.id ? 'create' : 'update',
            issueTypeId,
            fieldId: field.id,
            fieldOptionId: key,
            cascadeFieldId: chosenField?.id,
            defaultValue: !includes(selectTypes, chosenField?.fieldType) ? toJS(formatDate({ fieldCode: chosenField?.fieldCode, defaultValue, format: DATETIME })) : undefined,
            hidden,
            required,
            fieldCascadeRuleOptionList: getFieldCascadeRuleOptionList(chosenField?.fieldType, fieldRelOptionList, defaultValue),
          });
        });
      }
      const deleteCascadeRuleList = cascadeRuleList.filter((item) => !includes(map(cascadeRuleData, 'id'), item.id));
      deleteCascadeRuleList.forEach((item) => {
        cascadeRuleData.push({ ...item, _status: 'delete' });
      });
      await pageConfigApi.createCascadeRule(cascadeRuleData);
      if (onOk) {
        onOk();
      }
      return true;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [cascadeRuleList, dataSet, field.id, issueTypeId, linkagesMap, modal, onOk, prepareData]);

  const handleOptionChange = useCallback(async (id: string, needPrepareData = true) => {
    if (needPrepareData && !await prepareData()) {
      return;
    }
    switchOption(id);
  }, [prepareData, switchOption]);

  useEffect(() => {
    if (currentOptionId && linkagesMap.get(currentOptionId)?.length) {
      dataSet?.loadData(linkagesMap.get(currentOptionId));
    }
  }, [currentOptionId, dataSet, linkagesMap]);

  const linkToPageField = useCallback(() => {
    modal?.close();
    to(LINK_URL.pageField, {
      type: 'project',
    });
  }, [modal]);

  const currentRecord: Record | undefined = currentSelected ? dataSet?.find((record) => record.get('chosenField')?.id === currentSelected) : undefined;

  return (
    <div className={styles.linkage}>
      <Loading loading={loading} />
      <div className={styles.content}>
        <div className={styles.tip}>当前字段选择特定选项后，对应被联动的字段根据规则隐藏、或要求必填，变更为指定值。</div>
        <div className={styles.settings}>
          {
            hasOptions ? (
              <>
                <LinkageColumn
                  key="fieldOption"
                  title={`${field.name}字段选项`}
                  width={175}
                  contentStyle={{ marginLeft: -20, overflowY: 'hidden' }}
                >
                  <FieldOptions
                    field={field}
                    onChange={handleOptionChange}
                    currentOptionId={currentOptionId}
                    setHasOptions={setHasOptions}
                  />
                </LinkageColumn>
                <LinkageColumn key="chosenField" title="被关联字段" width={223} columnStyle={{ position: 'relative', paddingRight: 20 }}>
                  <ChosenFields
                    key={currentOptionId}
                    currentOptionId={currentOptionId}
                    dataSet={dataSet}
                    currentSelected={currentSelected}
                    setCurrentSelected={setCurrentSelected}
                    issueTypeId={issueTypeId}
                    fieldId={field.id}
                  />
                </LinkageColumn>
                <LinkageColumn key="rule" title="设置级联规则" bordered={false} columnStyle={{ flex: 1, paddingRight: 20 }}>
                  {currentRecord ? <Rule record={currentRecord} /> : null}
                </LinkageColumn>
              </>
            ) : (
              <EmptyPage
                description={(
                  <>
                    {`${field.name}字段尚未添加选项，请`}
                    <EmptyPage.Button
                      onClick={linkToPageField}
                    >
                      【设置选项】
                    </EmptyPage.Button>
                  </>
                  )}
                image={noData}
              />
            )
          }
        </div>
      </div>
    </div>
  );
};

const ObserverLinkage = observer(Linkage);

const openLinkage = (props: Props) => {
  Modal.open({
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    key: Modal.key(),
    title: '设置级联规则',
    okText: '保存',
    className: styles.linkage_modal,
    children: <ObserverLinkage {...props} />,
  });
};

export default openLinkage;
