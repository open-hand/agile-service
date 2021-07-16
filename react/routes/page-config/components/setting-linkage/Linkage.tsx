import React, {
  ReactElement, useCallback, useState, useEffect, useRef,
} from 'react';
import { Modal, Form, DataSet } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import Loading from '@/components/Loading';
import { IField, IModalProps } from '@/common/types';
import { ICascadeRule, pageConfigApi } from '@/api';
import { includes, map } from 'lodash';
import styles from './Linkage.less';
import FieldOptions from './components/field-options';
import ChosenFields from './components/chosen-fields';
import Rule from './components/Rule';
import { IChosenField } from './components/Rule/utils';

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
  field: IField
  modal?: IModalProps
  issueTypeId: string
}

export interface ICascadeLinkageSetting {
  chosenField: IField
  fieldRelOptionList?: {meaning: string, value: string}[]
  defaultValue?: any
  hidden?: boolean
  required?: boolean
}

const selectTypes = ['radio', 'checkbox', 'single', 'multiple'];

const Linkage: React.FC<Props> = ({ field, issueTypeId, modal }) => {
  const [loading, setLoading] = useState<boolean>(false);
  const [currentOptionId, setCurrentOptionId] = useState<string | undefined>(undefined);
  const [linkagesMap, setLinkagesMap] = useState<Map<string, ICascadeLinkageSetting[]>>(new Map());
  const [dataSet, setDataSet] = useState<DataSet>();
  const [currentSelected, setCurrentSelected] = useState<string | undefined>();
  const chosenFieldsRef = useRef<{ chosenFieldCodes: string[] } | null>(null);
  const [cascadeRuleList, setCascadeRuleList] = useState<ICascadeRule[]>([]);

  useEffect(() => {
    const getCascadeRuleList = async () => {
      const res = pageConfigApi.getCascadeRuleList(issueTypeId, field.id);
      setCascadeRuleList(res);
    };
    getCascadeRuleList();
  }, [field.id, issueTypeId]);

  const switchOption = useCallback((id: string) => {
    setCurrentOptionId(id);
    console.log(chosenFieldsRef?.current?.chosenFieldCodes);
    setCurrentSelected(chosenFieldsRef?.current?.chosenFieldCodes && chosenFieldsRef?.current?.chosenFieldCodes[0]);
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
        label: '隐藏优先级字段',
      }, {
        name: 'required',
        label: '设置为必填字段',
      }],
      events: {
        update: ({ name, record: current }: { name: string, record: Record }) => {
          if (name === 'chosenField') {
            current.init('statusId');
          }
        },
      },
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
    const handleOk = async () => {
      if (!await prepareData()) {
        return false;
      }
      const baseData: ICascadeRule[] = [];
      for (const [key, values] of linkagesMap.entries()) {
        values.forEach((value) => {
          const {
            chosenField, defaultValue, fieldRelOptionList, required, hidden,
          } = value;
          const originCascadeRule = cascadeRuleList.find((item) => item.fieldOptionId === key && item.cascadeFieldId === chosenField?.id) || { id: undefined };
          baseData.push({
            ...originCascadeRule,
            _status: !originCascadeRule.id ? 'create' : 'update',
            issueTypeId,
            fieldId: field.id,
            fieldOptionId: key,
            cascadeFieldId: chosenField?.id,
            defaultValue: !includes(selectTypes, chosenField?.fieldType) ? defaultValue : undefined,
            hidden,
            required,
            fieldCascadeRuleOptionList: (includes(selectTypes, chosenField?.fieldType) && fieldRelOptionList?.length) ? fieldRelOptionList.map((option) => ({
              cascadeOptionId: option.value,
              defaultOption: Array.isArray(defaultValue) ? includes(defaultValue, option.value) : option.value === defaultValue,
            })) : undefined,
          });
        });
      }
      const deleteCascadeRuleList = cascadeRuleList.filter((item) => !includes(map(baseData, 'id'), item.id));
      deleteCascadeRuleList.forEach((item) => {
        baseData.push({ ...item, _status: 'delete' });
      });
      console.log(baseData);
      // 刷新外边的表格
      return false;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [cascadeRuleList, dataSet, field.id, issueTypeId, linkagesMap, modal, prepareData]);

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

  const currentRecord: Record | undefined = currentSelected ? dataSet?.find((record) => record.get('chosenField')?.id === currentSelected) : undefined;

  console.log('render linkage', dataSet?.data, linkagesMap);
  return (
    <div className={styles.linkage}>
      <Loading loading={loading} />
      <div className={styles.content}>
        <div className={styles.tip}>当前字段选择特定选项后，对应被联动的字段根据规则隐藏、或要求必填，变更为指定值。</div>
        <div className={styles.settings}>
          <LinkageColumn
            key="fieldOption"
            title={`${field.name}字段选项`}
            width={175}
            contentStyle={{ marginLeft: -20, overflowY: 'hidden' }}
          >
            <FieldOptions field={field} onChange={handleOptionChange} currentOptionId={currentOptionId} />
          </LinkageColumn>
          <LinkageColumn key="chosenField" title="被关联字段" width={223} columnStyle={{ position: 'relative', paddingRight: 20 }}>
            <ChosenFields
              key={currentOptionId}
              dataSet={dataSet}
              currentSelected={currentSelected}
              setCurrentSelected={setCurrentSelected}
              issueTypeId={issueTypeId}
              fieldId={field.id}
              chosenFieldsRef={chosenFieldsRef}
            />
          </LinkageColumn>
          <LinkageColumn key="rule" title="设置级联规则" bordered={false} columnStyle={{ flex: 1, paddingRight: 20 }}>
            {currentRecord ? <Rule record={currentRecord} /> : null}
          </LinkageColumn>
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
