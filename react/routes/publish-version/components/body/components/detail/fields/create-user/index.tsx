import React from 'react';
import { observer } from 'mobx-react-lite';
import UserTag from '@/components/tag/user-tag';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import Field from '../field';

interface Props {

}
const CreateUser: React.FC<Props> = () => {
  const { store } = usePublishVersionContext();
  const { creationUser } = store.getCurrentData;
  return (
    <Field label="创建人">
      {creationUser ? <UserTag data={creationUser} /> : '无'}
    </Field>

  );
};

export default observer(CreateUser);
