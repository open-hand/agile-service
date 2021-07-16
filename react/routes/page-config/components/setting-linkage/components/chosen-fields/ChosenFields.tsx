import React, {
  useCallback, useEffect, useMemo, useState, useImperativeHandle,
} from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { DataSet } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import ChooseField, { useChoseField } from '@/components/chose-field';
import { pageConfigApi } from '@/api';
import classNames from 'classnames';
import styles from './ChosenFields.less';
import { IChosenField } from '../Rule/utils';
import useDeepMemo from './useDeepMemo';

interface Props {
  issueTypeId: string
  fieldId: string
  dataSet: DataSet | undefined,
  currentSelected: undefined | string
  setCurrentSelected: (selected: string | undefined) => void
  chosenFieldsRef: React.MutableRefObject<{chosenFieldCodes: string[]} | null>
}

const ChosenFields: React.FC<Props> = ({
  dataSet, currentSelected, setCurrentSelected, issueTypeId, fieldId, chosenFieldsRef,
}) => {
  const [chosenFields, setChosenFields] = useState<IChosenField[]>([]);
  const defaultValue = useDeepMemo(() => dataSet?.map((record) => record.get('chosenField')?.id));
  useEffect(() => {
    const getChosenFields = async () => {
      const res = await pageConfigApi.getCascadeFields(issueTypeId, fieldId);
      setChosenFields(res.map((item: any) => ({
        name: item.fieldName,
        id: item.fieldId,
        code: item.fieldId,
        system: item.createdLevel === 'system',
        fieldType: item.fieldType,
        fieldCode: item.fieldCode,
      })));
    };
    getChosenFields();
  }, [fieldId, issueTypeId]);
  const addRecord = useCallback((key) => {
    const newRecord = dataSet?.create();
    newRecord && newRecord.set('chosenField', chosenFields.find((field) => field.code === key));
  }, [dataSet, chosenFields]);

  const [choseDataProps, choseComponentProps] = useChoseField({
    fields: chosenFields,
    addFieldCallback: addRecord,
    // defaultValue,
  });

  // console.log('renderChosenFields', defaultValue);
  const { store: choseFieldStore } = choseDataProps;
  useEffect(() => {
    if (!currentSelected && choseFieldStore.getAllChosenField.length) {
      setCurrentSelected(choseFieldStore.getAllChosenField[0].code);
    }
  }, [choseFieldStore.getAllChosenField, currentSelected, setCurrentSelected]);

  const handleChangeSelected = useCallback((key) => {
    setCurrentSelected(key);
  }, [setCurrentSelected]);

  const handleCancelChosen = useCallback((e, key, record) => {
    e.stopPropagation();
    dataSet?.remove(record);
    choseFieldStore.delChosenFields(key);
    if ((currentSelected === key || !currentSelected) && choseFieldStore.getAllChosenField.length) {
      setCurrentSelected(choseFieldStore.getAllChosenField[0].id);
    }
    if (currentSelected === key && !choseFieldStore.getAllChosenField.length) {
      setCurrentSelected(undefined);
    }
    // 添加清楚相关rule的逻辑
  }, [choseFieldStore, currentSelected, dataSet, setCurrentSelected]);

  // useEffect(() => {
  //   if (dataSet?.length) {
  //     dataSet.forEach((record) => {
  //       const chosenField = record.get('chosenField');
  //       choseFieldStore.addChosenFields(chosenField?.id, chosenField);
  //     }, []);
  //   }
  // }, [choseFieldStore, dataSet]);

  useImperativeHandle(chosenFieldsRef, () => ({
    chosenFieldCodes: choseFieldStore.getAllChosenField?.map((item) => item.code),
  }));

  return (
    <div className={styles.chosenField}>
      <div className={styles.chosen_btn}>
        <ChooseField {...choseComponentProps} dropDownBtnProps={{ icon: 'add', style: { marginLeft: 6, marginTop: 10 } }} />
      </div>
      <div className={styles['option-list']}>
        {
          dataSet && dataSet.map((record) => {
            const chosenField = record.get('chosenField');
            return (
              <div
                className={classNames(styles['option-item'], {
                  [styles['option-item-selected']]: chosenField?.id === currentSelected,
                })}
                role="none"
                onClick={() => { handleChangeSelected(chosenField?.id); }}
              >
                <span className={styles['option-item-name']}>{chosenField?.name}</span>
                <span className={styles['option-item-clear']}>
                  <Icon type="close" onClick={(e) => { handleCancelChosen(e, chosenField?.id, record); }} />
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
