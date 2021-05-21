import React, {
  useMemo, useCallback, useEffect,
} from 'react';
import {
  DataSet, Form, Modal, TextField,
} from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import './index.less';
import { IModalProps } from '@/common/types';
import { observer } from 'mobx-react-lite';

interface IImportPomFunctionProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
  data: any
}

const EditAppVersionModal: React.FC<{ modal?: IModalProps } & Partial<IImportPomFunctionProps>> = ({ modal, handleOk, data }) => {
  const formDs = useMemo(() => new DataSet({
    autoCreate: true,
    data: data ? [data] : undefined,
    fields: [
      {
        name: 'tagAlias', label: '版本别名', required: false, maxLength: 16,
      },
      // { name: 'serviceCode', label: '关联应用服务' },
      // { name: 'tagName', label: '关联tag' },
    ],
  }), [data]);

  const handleSubmit = useCallback(async () => {
    if (!data && !await formDs.validate()) {
      return false;
    }
    const result = handleOk && await handleOk(formDs.current?.toData());
    return typeof (result) !== 'undefined' ? result : true;
  }, [data, formDs, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={formDs}>
      <TextField name="tagAlias" />
    </Form>
  );
};
const ObserverEditAppVersionModal = observer(EditAppVersionModal);
async function openEditAppVersionModal(props: IImportPomFunctionProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '修改关联版本别名',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <ObserverEditAppVersionModal {...props} />,
  });
}

export { openEditAppVersionModal };
