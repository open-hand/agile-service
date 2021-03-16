import React, {
  useMemo, useCallback, useEffect,
} from 'react';
import {
  DataSet, Form, Modal, Table, TextField,
} from 'choerodon-ui/pro/lib';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import './index.less';
import { IModalProps } from '@/common/types';
import { IAppVersionData, versionApiConfig } from '@/api';
import { observer } from 'mobx-react-lite';

interface IImportPomFunctionProps {
    handleOk?: ((data: any) => void) | (() => Promise<any>)
    data: IAppVersionData
}

const EditAppVersionModal: React.FC<{ modal?: IModalProps } & IImportPomFunctionProps> = ({ modal, handleOk, data }) => {
  const formDs = useMemo(() => new DataSet({
    autoCreate: true,
    data: [data],
    fields: [
      { name: 'versionAlias', label: '版本别名' },
      { name: 'version', label: '版本名称' },
    ],
    transport: {
      submit: ({ data: newData }) => versionApiConfig.updateAppVersion(newData[0], data.id!),
    },
  }), [data]);

  const handleSubmit = useCallback(async () => {
    if (!await formDs.submit()) {
      return false;
    }
    const result = handleOk && await handleOk(data);
    return typeof (result) !== 'undefined' ? result : true;
  }, [formDs, handleOk]);
  useEffect(() => {
        modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={formDs}>
      <TextField name="version" />
      <TextField name="versionAlias" />
    </Form>
  );
};
const ObserverEditAppVersionModal = observer(EditAppVersionModal);
function openEditAppVersionModal(props: IImportPomFunctionProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '修改应用版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <ObserverEditAppVersionModal {...props} />,
  });
}
export { openEditAppVersionModal };
