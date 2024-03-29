import React, {
  useCallback, useEffect, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import { DataSet, Icon } from 'choerodon-ui/pro';
import ChooseField, { useChoseField } from '@/components/chose-field';
import { FuncType } from 'choerodon-ui/pro/lib/button/interface';
import { pageConfigApi } from '@/api';
import classNames from 'classnames';
import useDeepMemo from '@/hooks/useDeepMemo';
import styles from './ChosenFields.less';
import { IChosenField } from '../Rule/utils';

interface Props {
  issueTypeId: string
  fieldId: string
  dataSet: DataSet | undefined,
  currentSelected: undefined | string
  setCurrentSelected: (selected: string | undefined) => void
  currentOptionId: string | undefined
}

const ChosenFields: React.FC<Props> = ({
  dataSet, currentSelected, setCurrentSelected, issueTypeId, fieldId, currentOptionId,
}) => {
  const [chosenFields, setChosenFields] = useState<IChosenField[]>([]);
  const chosenFieldIds = useDeepMemo(() => dataSet?.map((record) => record.get('chosenField')?.id));
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
    value: chosenFieldIds,
  });

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
  }, [choseFieldStore, currentSelected, dataSet, setCurrentSelected]);

  return (
    <div className={styles.chosenField}>
      <div className={styles.chosen_btn}>
        <ChooseField
          {...choseComponentProps}
          dropDownProps={{ disabled: !currentOptionId }}
          dropDownBtnProps={{ icon: 'add', funcType: 'flat' as FuncType, style: { marginLeft: 6, marginTop: 9 } }}
          dropDownBtnChildren="添加字段"
        />
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
