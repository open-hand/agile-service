export interface User {
  email: string
  enabled?: boolean
  id: number
  imageUrl: string | null
  ldap: boolean
  loginName: string
  realName: string
}

interface Issue {
  issueId: string
  summary: string
}

export interface PI {
  code: string
  name: string
}
