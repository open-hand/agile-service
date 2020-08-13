import React, {
  useEffect, useMemo, useCallback, useState,
} from 'react';
import {
  Form, DataSet, Spin, Modal,
} from 'choerodon-ui/pro';
import { find } from 'lodash';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { ITotalStatus, statusTransformApi } from '@/api';
import SelectStatus from '@/components/select/select-status';
import './index.less';
import { useIssueTypes } from '@/hooks';

interface Props {
  onSubmit: Function
  data: ITotalStatus
  modal?: any
}
interface NeedTransform {
  issueTypeId: string
  issueTypeName: string
}
const DeleteStatus: React.FC<Props> = ({
  modal, onSubmit, data,
}) => {
  const [loading, setLoading] = useState(false);
  const [needTransforms, setNeedTransforms] = useState<NeedTransform[]>([]);
  const [issueTypes] = useIssueTypes();
  useEffect(() => {
    (async () => {
      if (data.usage) {
        modal.update({
          okProps: {
            disabled: true,
          },
        });
        setLoading(true);
        const res: string[] = await statusTransformApi.checkStatusNeedTransform(data.id);
        setNeedTransforms(res.map((issueTypeId: string) => {
          const target = find(issueTypes, { id: issueTypeId });
          return {
            issueTypeId,
            issueTypeName: target?.name || '',
          };
        }));
        setLoading(false);
        modal.update({
          okProps: {
            disabled: false,
          },
        });
      }
    })();
  }, [data.id, data.usage]);
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: needTransforms.map((needTransform) => ({
      name: needTransform.issueTypeId,
      type: 'string' as FieldType,
      label: `使用该状态的${needTransform.issueTypeName}变更为`,
      required: true,
      textField: 'name',
      valueField: 'id',
    })),
  }), [needTransforms]);

  const handleSubmit = useCallback(async () => {
    try {
      if (!await dataSet.validate()) {
        return false;
      }
      const selectData = dataSet.toData()[0] as { [issueTypeId: string]: string };
      const transforms = Object.keys(selectData).map((issueTypeId) => ({
        issueTypeId,
        statusId: selectData[issueTypeId],
      }));
      await statusTransformApi.deleteStatus(data.id, transforms);
      onSubmit();
      return true;
    } catch (error) {
      return false;
    }
  }, [data.id, dataSet]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return (
    <Spin spinning={loading}>
      {needTransforms.length > 0 && (
        <Form dataSet={dataSet}>
          {needTransforms.map((needTransform) => (
            <SelectStatus
              name={needTransform.issueTypeId}
              issueTypeId={needTransform.issueTypeId}
            />
          ))}
        </Form>
      )}
    </Spin>
  );
};
const openDeleteStatus = (props: Omit<Props, 'modal'>) => {
  Modal.open({
    key: 'delete',
    title: `确认删除状态“${props.data.name}”`,
    children: <DeleteStatus {...props} />,
  });
};

export default openDeleteStatus;
