import openCreateIssueModal, { CreateIssueProps } from '@/components/create-issue';
import { CreateIssueBaseProps } from '../create-issue/BaseComponent';

export type CreateSubTaskProps = CreateIssueProps & Required<Pick<CreateIssueBaseProps, 'parentIssue'>>

const openModal = (props: CreateSubTaskProps) => {
  openCreateIssueModal({ typeCode: 'sub_task', isProgram: false, ...props });
};
export default openModal;
