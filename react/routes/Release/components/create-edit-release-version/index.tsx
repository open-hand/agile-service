import React, { useCallback, useEffect, useMemo } from 'react';
import {
  DataSet, DatePicker, Form, Modal, TextField,
} from 'choerodon-ui/pro';
import { FieldTrim } from 'choerodon-ui/pro/lib/data-set/enum';
import TextArea from '@/components/TextArea';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps } from '@/common/types';
import { versionApi } from '@/api';
import moment from 'moment';
import { getProjectId } from '@/utils/common';

interface PublishVersionModalProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
}
interface PublishVersionModalWithEditProps extends PublishVersionModalProps {
  editData: any
}

const CreatePublishVersion: React.FC<{ modal?: IModalProps } & Partial<PublishVersionModalWithEditProps>> = ({
  modal, handleOk, editData,
}) => {
  const handleCheckName = useCallback(async (value: string) => {
    if (value && value.trim()) {
      return editData?.name === value.trim() ? true : versionApi.checkName(value.trim()).then((res: any) => {
        if (res) {
          return '版本名称重复';
        }
        return true;
      }).catch(() => {
      });
    }
    return true;
  }, [editData?.name]);
  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: false,
    paging: false,
    fields: [
      {
        name: 'name', label: '版本名称', required: true, validator: handleCheckName, maxLength: 15, trim: 'both' as FieldTrim,
      },
      {
        name: 'startDate', label: '开始日期', max: 'expectReleaseDate', defaultValue: moment().format('YYYY-MM-DD'),
      },
      { name: 'expectReleaseDate', label: '预计发布日期', min: 'startDate' },

      { name: 'description', label: '版本描述', maxLength: 30 },

    ],
  }), [handleCheckName]);
  useEffect(() => {
    editData ? ds.loadData([{
      ...editData,
      expectReleaseDate: editData.expectReleaseDate ? String(editData.expectReleaseDate).split(' ')[0] : undefined,
      startDate: editData.startDate ? String(editData.startDate).split(' ')[0] : undefined,
    }]) : ds.create();
  }, [ds, editData]);
  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }
    const values = ds.current?.toJSONData();
    const { objectVersionNumber } = editData || {};
    const data = {
      objectVersionNumber,
      description: values.description,
      name: values.name.trim(),
      projectId: getProjectId(),
      startDate: values.startDate ? `${moment(values.startDate, 'YYYY-MM-DD').format('YYYY-MM-DD')} 00:00:00` : null,
      expectReleaseDate: values.expectReleaseDate ? `${moment(values.expectReleaseDate, 'YYYY-MM-DD').format('YYYY-MM-DD')} 00:00:00` : null,
    };
    editData ? await versionApi.update(editData.versionId, data) : await versionApi.create(data);
    const result = handleOk && await handleOk({ ...editData, ...data });
    return typeof (result) !== 'undefined' ? result : true;
  }, [ds, editData, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds}>
      <TextField name="name" showLengthInfo />
      <DatePicker name="startDate" />
      <DatePicker name="expectReleaseDate" />
      <TextArea name="description" rows={3} maxLength={30} />
    </Form>
  );
};
function openCreateReleaseVersionModal(props: PublishVersionModalProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '创建规划版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <CreatePublishVersion {...props} />,

  });
}
function openEditReleaseVersionModal(props: PublishVersionModalWithEditProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '修改规划版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <CreatePublishVersion {...props} />,
    okText: '保存',
  });
}
export { openCreateReleaseVersionModal, openEditReleaseVersionModal };
