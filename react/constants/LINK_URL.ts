interface IRouterList {
  knowledgeDoc: (baseId: string) => string,
  workListIssue: '/agile/work-list/issue',
  workListBacklog: '/agile/work-list/backlog',
}
const LINK_URL: IRouterList = {
  knowledgeDoc: (baseId: string) => `/knowledge/project/doc/${baseId}`,
  workListIssue: '/agile/work-list/issue',
  workListBacklog: '/agile/work-list/backlog',
};
export default LINK_URL;
