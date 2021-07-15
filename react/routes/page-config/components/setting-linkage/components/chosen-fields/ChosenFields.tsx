import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { DataSet } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import ChooseField, { useChoseField } from '@/components/chose-field';
import useIsProgram from '@/hooks/useIsProgram';
import { IField } from '@/common/types';
import { fieldApi } from '@/api';
import classNames from 'classnames';
import styles from './ChosenFields.less';

const programSystemFields = [
  {
    code: 'teamProjectList',
    name: '负责团队',
    defaultShow: true,
    fieldType: 'multiple',
  },
  {
    code: 'reporterList',
    name: '报告人',
    defaultShow: false,
    fieldType: 'member',
  }, {
    code: 'createDate',
    name: '创建时间',
    defaultShow: false,
    fieldType: 'datetime',
  },
  {
    code: 'estimatedStartTime',
    name: '预计开始时间',
    defaultShow: false,
    fieldType: 'datetime',
  },
  {
    code: 'acceptanceCritera',
    name: '验收标准',
    defaultShow: false,
    fieldType: 'text',
  },
  {
    code: 'benfitHypothesis',
    name: '特性价值',
    defaultShow: false,
    fieldType: 'text',
  },
];

const systemFields = [{
  code: 'assigneeId',
  name: '经办人',
  defaultShow: true,
  fieldType: 'member',
}, {
  code: 'reporterIds',
  name: '报告人',
  defaultShow: false,
  fieldType: 'member',
}, {
  code: 'component',
  name: '模块',
  defaultShow: false,
  fieldType: 'multiple',
}, {
  code: 'priorityId',
  name: '优先级',
  defaultShow: true,
  fieldType: 'multiple',
}, {
  code: 'fixVersion',
  name: '修复的版本',
  fieldType: 'multiple',
}, {
  code: 'influenceVersion',
  name: '影响的版本',
  fieldType: 'multiple',
},
{
  code: 'estimatedStartTime',
  name: '预计开始时间',
  defaultShow: false,
  fieldType: 'datetime',
},
{
  code: 'estimatedEndTime',
  name: '预计结束时间',
  defaultShow: false,
  fieldType: 'datetime',
},
{
  code: 'mainResponsibleIds',
  name: '主要负责人',
  defaultShow: false,
  fieldType: 'member',
}, {
  code: 'environment',
  name: '环境',
  defaultShow: false,
  fieldType: 'multiple',
}];

interface Props {
  dataSet: DataSet | undefined,
  currentSelected: undefined | string
  setCurrentSelected: (selected: string | undefined) => void
}

const ChosenFields: React.FC<Props> = ({ dataSet, currentSelected, setCurrentSelected }) => {
  const [customFields, setCustomFields] = useState<IField[]>([]);
  const { isProgram } = useIsProgram();
  const fields = useMemo(() => [...customFields, ...(isProgram ? programSystemFields : systemFields)], [customFields, isProgram]);
  useEffect(() => {
    const getCustomFields = async () => {
      const res = await fieldApi.getCustomFields();
      setCustomFields(res);
    };
    getCustomFields();
  }, []);
  const addRecord = useCallback((key) => {
    const newRecord = dataSet?.create();
    newRecord && newRecord.set('chosenOption', fields.find((field) => field.code === key));
  }, [dataSet, fields]);
  const [choseDataProps, choseComponentProps] = useChoseField({
    fields,
    addFieldCallback: addRecord,
  });
  const { store: choseFieldStore } = choseDataProps;
  useEffect(() => {
    if (!currentSelected && choseFieldStore.getAllChosenField.length) {
      setCurrentSelected(choseFieldStore.getAllChosenField[0].code);
    }
  }, [choseFieldStore.getAllChosenField, currentSelected, setCurrentSelected]);

  const handleChangeSelected = useCallback((code) => {
    setCurrentSelected(code);
  }, [setCurrentSelected]);

  const handleCancelChosen = useCallback((e, code, record) => {
    e.stopPropagation();
    dataSet?.remove(record);
    choseFieldStore.delChosenFields(code);
    if ((currentSelected === code || !currentSelected) && choseFieldStore.getAllChosenField.length) {
      setCurrentSelected(choseFieldStore.getAllChosenField[0].code);
    }
    if (currentSelected === code && !choseFieldStore.getAllChosenField.length) {
      setCurrentSelected(undefined);
    }
    // 添加清楚相关rule的逻辑
  }, [choseFieldStore, currentSelected, dataSet, setCurrentSelected]);

  console.log(toJS(choseFieldStore.getAllChosenField), dataSet, currentSelected);
  return (
    <div className={styles.chosenOption}>
      <div className={styles.chosen_btn}>
        <ChooseField {...choseComponentProps} dropDownBtnProps={{ icon: 'add', style: { marginLeft: 6, marginTop: 10 } }} />
      </div>
      <div className={styles['option-list']}>
        {/* {
          choseFieldStore.getAllChosenField.map((field) => (
            <div
              className={classNames(styles['option-item'], {
                [styles['option-item-selected']]: field.code === currentSelected,
              })}
              role="none"
              onClick={() => { handleChangeSelected(field.code); }}
            >
              <span className={styles['option-item-name']}>{field.name}</span>
              <span className={styles['option-item-clear']}>
                <Icon type="close" onClick={(e) => { handleCancelChosen(e, field.code); }} />
              </span>
            </div>
          ))
        } */}
        {
          dataSet && dataSet.map((record) => {
            const chosenField = record.get('chosenOption');
            return (
              <div
                className={classNames(styles['option-item'], {
                  [styles['option-item-selected']]: chosenField?.code === currentSelected,
                })}
                role="none"
                onClick={() => { handleChangeSelected(chosenField?.code); }}
              >
                <span className={styles['option-item-name']}>{chosenField?.name}</span>
                <span className={styles['option-item-clear']}>
                  <Icon type="close" onClick={(e) => { handleCancelChosen(e, chosenField?.code, record); }} />
                </span>
              </div>
            );
          })
        }
      </div>
    </div>
  );
};

export default observer(ChosenFields);
