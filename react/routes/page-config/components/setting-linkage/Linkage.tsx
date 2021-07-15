import React, { ReactElement, useCallback, useState } from 'react';
import { Modal, Form, DataSet } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import Loading from '@/components/Loading';
import { IField } from '@/common/types';
import styles from './Linkage.less';
import FieldOptions from './components/field-options';
import ChosenFields from './components/chosen-fields';
import Rule from './components/Rule';

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
}

export interface IFeatureLinkageSetting {
  id: string
  chosenFieldCode: string
  projectId: string
  statusId: string
}

const Linkage: React.FC<Props> = ({ field }) => {
  const [loading, setLoading] = useState<boolean>(false);
  const [currentOptionId, setCurrentOptionId] = useState<string | undefined>(undefined);
  const [linkagesMap, setLinkagesMap] = useState<Map<string, IFeatureLinkageSetting[]>>(new Map());
  const [dataSet, setDataSet] = useState<DataSet>();
  const [currentSelected, setCurrentSelected] = useState<string | undefined>();

  const switchOption = useCallback((id: string) => {
    setCurrentOptionId(id);
    setDataSet(new DataSet({
      fields: [{
        name: 'chosenOption',
        label: '被关联字段',
      }, {
        name: 'fieldRelOptionList',
        label: '可见选项',
        textField: 'value',
        valueField: 'id',
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
          if (name === 'chosenFieldCode') {
            current.init('statusId');
          }
        },
      },
    }));
  }, []);
  const prepareData = useCallback(async () => {
    const hasValue = dataSet?.find((r) => r.get('chosenFieldCode'));
    if (hasValue && !await dataSet?.validate()) {
      return false;
    }
    const data: any = dataSet?.toData();
    linkagesMap.set(currentOptionId as string, hasValue ? data.map((item: any) => ({ optionId: currentOptionId, ...item })) : []);
    return true;
  }, [currentOptionId, dataSet, linkagesMap]);

  const handleOptionChange = useCallback(async (id: string, needPrepareData = true) => {
    if (needPrepareData && !await prepareData()) {
      return;
    }
    switchOption(id);
  }, [prepareData, switchOption]);

  const currentRecord: Record | undefined = currentSelected ? dataSet?.find((record) => record.get('chosenOption')?.code === currentSelected) : undefined;

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
          <LinkageColumn key="chosenOption" title="被关联字段" width={223} columnStyle={{ position: 'relative', paddingRight: 20 }}>
            <ChosenFields dataSet={dataSet} currentSelected={currentSelected} setCurrentSelected={setCurrentSelected} />
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
