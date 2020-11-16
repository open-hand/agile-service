import { IModalProps } from '@/common/types';
import {
  DataSet, Form, Select, Modal,
} from 'choerodon-ui/pro';
import React, { useCallback, useEffect, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { versionApiConfig } from '@/api';
import { IsInProgram } from '@/hooks/useIsInProgram';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import dataSet from '@/components/issue-table/dataSet';

interface Props {
  modal?: IModalProps,
  programId: string
  versionId: string
  defaultValue?:any
  onRefresh: () => void
}
const LinkProgramVersion: React.FC<Props> = (props) => {
  const ds = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'programVersion',
      label: '项目群版本',
      textField: 'name',
      valueField: 'id',
      defaultValue: props.defaultValue,
      options: new DataSet({
        autoQuery: true,
        fields: [{ name: 'id', type: 'string' as FieldType }, { name: 'name', type: 'string' as FieldType }],
        transport: {
          read: versionApiConfig.loadProgramVersion(props.programId),
        },
      }),
    }],
    transport: {
      submit: ({ data, params }) => ({
        ...versionApiConfig.linkProgramVersion(data[0].programVersion, props.versionId),
        data: null,
      }),
    },
  }), [props.defaultValue, props.programId, props.versionId]);
  const handleOnOk = useCallback(async () => {
    if (await ds.current?.validate()) {
      ds.submit().then(() => {
        props.onRefresh && props.onRefresh();
      });
      return true;
    }
    return false;
  }, []);
  useEffect(() => {
    props.modal?.handleOk(handleOnOk);
  }, []);
  return (
    <Form dataSet={ds}>
      <Select name="programVersion" />
    </Form>
  );
};
const ObserverLinkProgramVersion = observer(LinkProgramVersion);
const openLinkVersionModal = (versionId: string, programId: string, programVersion: { name: string, id: string } | undefined, onRefresh: () => void) => {
  Modal.open({
    key: 'LinkPiAimModal',
    title: '关联项目群版本',
    drawer: true,
    style: {
      width: MODAL_WIDTH.small,
    },
    children: <ObserverLinkProgramVersion versionId={versionId} programId={programId} onRefresh={onRefresh} defaultValue={programVersion?.id} />,
  });
};
export { openLinkVersionModal };
