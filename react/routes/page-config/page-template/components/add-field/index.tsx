import React, { useState, useEffect } from 'react';
import {
  Form, Select, DataSet, Modal,
} from 'choerodon-ui/pro';
import { isEmpty } from 'lodash';
import { C7NFormat } from '@choerodon/master';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { IModalProps } from '@/common/types';
import { pageConfigApi } from '@/api';
import PageTemplateStore from '../../stores/PageTemplateStore';

const { Option } = Select;
interface Props {
  modal?: IModalProps,
  dataSet: DataSet,
  store: PageTemplateStore,
  onSubmitLocal: any,
  onRestoreLocal: (record: Record) => Promise<boolean>,
}
interface IPage {
  name: string,
  fieldId?: string,
  id: string,
}
const defaultValueFieldType = ['multiple', 'checkbox', 'multiMember'];
const AddFiled: React.FC<Props> = observer(({
  modal, dataSet, store, onSubmitLocal, onRestoreLocal,
}) => {
  const [pageList, setPageList] = useState([] as IPage[]);
  async function handleSubmit() {
    if (dataSet.validate()) {
      const id = dataSet.current?.toData().field;
      const addFiledData = store.currentTypeAllFieldData.get(id);
      if (addFiledData) {
        onSubmitLocal({
          ...addFiledData,
          defaultValue: !isEmpty(addFiledData.defaultValue) && defaultValueFieldType.includes(addFiledData.fieldType) ? String(addFiledData.defaultValue).split(',') : addFiledData.defaultValue,
          fieldOptions: addFiledData.fieldOptions || addFiledData.defaultValueObj,
          localRecordIndexId: dataSet.current?.index,
        }, true);
      } else {
        const deleteRecord = store.getDeleteRecords.find((record) => record.get('id') === id);
        deleteRecord && onRestoreLocal(deleteRecord);
      }
      dataSet.create();
      return true;
    }
    return false;
  }
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, []);
  useEffect(() => {
    pageConfigApi.loadUnSelected(store.getCurrentIssueType).then((res) => {
      store.loadCurrentTypeAllField(res);
      const currentDataArr = dataSet.toData();
      const deleteRecords = store.getDeleteRecords.map((record) => {
        const recordData = record.toData();
        return {
          ...recordData,
          name: recordData.fieldName,
          deleteAgainAdd: true, // 标记创建过的字段重新增添
        };
      });
      // 第一次进入时不进行过滤 并且会过滤已增添字段  过滤系统字段
      const data = res.filter((item) => currentDataArr.length === 1
        || !currentDataArr.some((d: any) => d.field === item.id));
      setPageList(data.concat(deleteRecords));
    });
    return () => {
      dataSet.current?.reset();
    };
  }, []);
  return (
    <Form record={dataSet.current}>
      <Select name="field">
        {pageList.map((item) => <Option value={item.id}>{item.name}</Option>)}
      </Select>
    </Form>
  );
});

const openField = (dataSet: DataSet, store: PageTemplateStore, onSubmitLocal: any, onRestoreLocal: Props['onRestoreLocal']) => {
  Modal.open({
    key: Modal.key(),
    title: <C7NFormat
      intlPrefix="agile.page"
      id="add.exist"
    />,
    style: {
      width: 340,
    },
    drawer: true,
    children: <AddFiled
      dataSet={dataSet}
      store={store}
      onSubmitLocal={onSubmitLocal}
      onRestoreLocal={onRestoreLocal}
    />,
  });
};
export default openField;
