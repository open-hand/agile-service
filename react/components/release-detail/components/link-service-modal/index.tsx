import React, { useMemo, useState } from 'react';
import {
  DataSet, Form, Modal, Select,
} from 'choerodon-ui/pro/lib';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';

interface ILinkServiceProps {
  handleOk?: (data: any) => void
}
const LinkService: React.FC = () => {
  const [applicationId, setApplicationId] = useState<string>();
  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: true,
    paging: false,
    // data: [
    //   { appService: '应用1', alias: undefined },
    // ],
    fields: [
      { name: 'appService', label: '选择应用服务', required: true },
      { name: 'tag', label: '选择tag', required: true },

    ],
  }), []);
  return (
    <Form dataSet={ds}>
      <SelectAppService name="appService" onChange={setApplicationId} />
      <SelectGitTags name="tag" applicationId={applicationId} key={`git-tags-${applicationId}`} />
    </Form>
  );
};
function openLinkServiceModal(props?:ILinkServiceProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '关联应用版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <LinkService />,

  });
}
export { openLinkServiceModal };
