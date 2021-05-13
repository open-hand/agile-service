import React, { useCallback, useEffect, useMemo } from 'react';
import {
  DataSet, DatePicker, Form, Modal, TextField, TextArea,
} from 'choerodon-ui/pro';
import { pick } from 'lodash';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps } from '@/common/types';
import { IPublishVersionData, publishVersionApi } from '@/api';

interface PublishVersionModalProps {
  handleOk?: ((data: Partial<IPublishVersionData> & Required<Pick<IPublishVersionData, 'versionAlias'>>) => void) | (() => Promise<any>)
}
interface PublishVersionModalWithEditProps extends PublishVersionModalProps {
  editData: IPublishVersionData
}

const CreatePublishVersion: React.FC<{ modal?: IModalProps } & Partial<PublishVersionModalWithEditProps>> = ({
  modal, handleOk, editData,
}) => {
  const handleCheckName = useCallback((newName: string) => publishVersionApi.checkAlias(newName, editData?.id).then((res: boolean) => (res ? '版本名称重复' : true)), [editData?.id]);
  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: false,
    paging: false,
    fields: [
      {
        name: 'versionAlias', label: '发布版本名称', required: true, validator: handleCheckName,
      },
      { name: 'actualPublishDate', label: '发布时间' },
      { name: 'description', label: '描述' },

    ],
  }), [handleCheckName]);
  useEffect(() => {
    editData ? ds.loadData([editData]) : ds.create();
  }, [ds, editData]);
  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }
    const data = pick(ds.current?.toData(), ['versionAlias', 'actualPublishDate', 'description']);

    const result = handleOk && await handleOk({ ...editData, ...data });
    return typeof (result) !== 'undefined' ? result : true;
  }, [ds, editData, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds}>
      <TextField name="versionAlias" />
      <DatePicker name="actualPublishDate" />
      <TextArea name="description" />
    </Form>
  );
};
function openCreatePublishVersionModal(props: PublishVersionModalProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '创建发布版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <CreatePublishVersion {...props} />,

  });
}
function openEditPublishVersionModal(props: PublishVersionModalWithEditProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '编辑发布版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <CreatePublishVersion {...props} />,

  });
}
export { openCreatePublishVersionModal, openEditPublishVersionModal };
