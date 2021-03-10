import { IVersionStatusCode } from '@/common/types';

const VERSION_STATUS_TYPE: {
  [key in IVersionStatusCode]: {
    color: string
    name: string
  }
} = {
  version_planning: { color: '#ffb100', name: '规划中' },
  released: { color: '#00bfa5', name: '已发布' },
  archived: { color: 'rgba(0, 0, 0, 0.3)', name: '归档' },
};
export default VERSION_STATUS_TYPE;
