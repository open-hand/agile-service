import { findIndex } from 'lodash';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { User } from '@/common/types';

interface Props {
  type: string,
  id: string,
  defaultUserId: string | number,
  isEdit: boolean,
}
const OptionDatSet = ({
  type, id, defaultUserId, isEdit,
}: Props): DataSetProps => ({
  autoQuery: true,
  paging: false,
  transport: {
    read: ({ data: p, dataSet, params }) => ({
      // defaultUserId 初始值（初次进入编辑框未修改时）
      url: `/iam/choerodon/v1/${type}s/${id}/users?size=10${isEdit && p.userId === defaultUserId ? `&userId=${defaultUserId}` : ''}`,
      method: 'get',
      transformResponse: (response) => {
        try {
          const data = JSON.parse(response);
          if (data && data.content) {
            const oldIndex = findIndex(dataSet?.toData(), (item: User) => item.id === p.userId);
            const index = findIndex(data.content, (item: User) => item.id === p.userId);
            if (index === -1 && oldIndex !== -1) {
              data.content.unshift(dataSet?.get(oldIndex)?.toData());
            }
            return data.content;
          }
          return data;
        } catch (error) {
          return response;
        }
      },
    }),
  },
});
export default OptionDatSet;
