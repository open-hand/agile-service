import { IVersionStatusCode } from '@/common/types';

const PUBLISH_VERSION_STATUS_TYPE: {
  [key in 'version_planning'|'released']: {
    color: string
    name: string
  }
} = {
  version_planning: { color: '#ffb100', name: '未发布' },
  released: { color: '#00bfa5', name: '已发布' },
};
export default PUBLISH_VERSION_STATUS_TYPE;
